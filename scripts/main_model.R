pacman::p_load(bets, tidyverse)

log_in_pinnacle()

coefs <- bets::lasso_coefs
configs <- bets::lasso_models %>%
  left_join(coefs, by = c('model_id' = 'term')) %>%
  arrange(desc(estimate))

league_specs <-
  tibble(league.id = c(2436, 2386, 1928, 2196, 1980, 2432, 2036, 1842, 1977, 1843, 2242, 2663, 1834, 210697),
         league = c('I1', 'P1', 'N1', 'SP1', 'E0', 'SP2', 'F1', 'D1', 'E1', 'D2', 'Liga MX', 'MLS', 'Serie A', 'Liga Profesional'))

pinnacle_odds <- get_pinnacle_odds(league_specs$league.id) %>%
  rename(league.id = leagues.id) %>%
  group_by(league.id) %>%
  nest(pelit = c(-league.id)) %>%
  left_join(league_specs)
pinnacle_odds

main_leagues <- bets::get_main_leagues('https://www.football-data.co.uk/mmz4281/2223/all-euro-data-2022-2023.xlsx',
                                       pinnacle_odds$league)
new_leagues <- bets::get_extra_leagues('https://www.football-data.co.uk/new/new_leagues_data.xlsx',
                                       pinnacle_odds$league)

buch_leagues <- bind_rows(main_leagues, new_leagues)
buch_leagues

active_leagues <- buch_leagues %>%
  group_by(league) %>%
  summarise(days_since_last = as.double(Sys.Date()-last(date))) %>%
  filter(days_since_last < 20) %>%
  pull(league)

pinnacle_odds <- pinnacle_odds %>%
  filter(league %in% active_leagues)

fbref_countries <- c("ENG", "ESP", "ITA", "GER", "FRA", 'USA', 'NED', 'MEX', 'POR', 'BRA', 'ENG')
latest_comps <- vroom::vroom('https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv') %>%
  filter(country %in% fbref_countries, gender == 'M') %>%
  select(competition_type, competition_name, country, last_season, season_end_year) %>%
  distinct() %>%
  filter(str_detect(competition_type, '1st|2nd')) %>%
  group_by(competition_type, competition_name) %>%
  filter(season_end_year == max(season_end_year))

fbref_leagues <- get_fbref_data(seasons = 2023) %>%
  filter(!is.na(hg))

fbref_leagues %>%
  group_by(league) %>%
  summarise(max_date = max(date))

main_data <- join_buch_fbref(buch_data = buch_leagues, fbref_data = fbref_leagues) %>%
  select(-contains('date_'), -c(PSCH:FTR)) %>%
  filter(league %in% active_leagues)

main_data %>%
  select(league, h_xg) %>%
  group_by(league) %>%
  skimr::skim()

main_data <- main_data %>%
  mutate(h_xg = if_else(is.na(h_xg), mhxg, h_xg),
         a_xg = if_else(is.na(a_xg), maxg, a_xg))

main_data %>%
  select(league, h_xg) %>%
  group_by(league) %>%
  skimr::skim()

# muutetaan pinnacle nimet
pin_names <- main_data %>%
  replace_team_names(home, away, pattern = team_dictionary()$buch_name, replacement = team_dictionary()$pin_name)

fits <- pin_names %>%
  group_nest(league) %>%
  filter(map_dbl(data, nrow) >= 60) %>% #min amount of games played
  unnest(data) %>%
  mutate(configs = list(configs %>% select(-penalty))) %>%
  unnest(c(configs)) %>%
  mutate(hwxg = wmkt*mhxg + wxg*h_xg + wgoals*FTHG,
         awxg = wmkt*maxg + wxg*a_xg + wgoals*FTAG) %>%
  group_nest(league, model_id, xi) %>%
  mutate(fit = map2(data, xi, fit_multimixture_model)) %>%
  select(league, model_id, fit) %>%
  mutate(teams = map(fit, ~.x %>% pluck('all_teams')))
fits

suppressWarnings(
  preds <- fits %>%
    left_join(pinnacle_odds) %>%
    mutate(pelit = map2(fit, pelit, possibly(predict_1x2, otherwise = NA))) %>%
    select(league, model_id, pelit, teams) %>%
    unnest(pelit)
)

team_names_map <- preds %>%
  filter(is.na(p1)) %>%
  select(league, team1, team2, teams) %>%
  distinct() %>%
  group_nest(league, teams) %>%
  mutate(pin_home = map(data, ~ .x %>% pull(team1)),
         pin_away = map(data, ~ .x %>% pull(team2)),
         pin_teams = map2(pin_home, pin_away, ~c(.x, .y))) %>%
  select(league, teams, pin_teams) %>%
  unnest(c(teams)) %>%
  unnest(c(pin_teams)) %>%
  mutate(correct = if_else(pin_teams %in% teams, TRUE, FALSE)) %>%
  filter(correct == FALSE) %>%
  select(-correct) %>%
  group_nest(league, pin_teams) %>%
  arrange(league, pin_teams) %>%
  group_by(league) %>%
  mutate(closest_match = map_chr(pin_teams, function(x) {
    teams <- data[[1]]$teams
    distances <- adist(x, teams)
    teams[which.min(distances)]
  })) %>%
  ungroup() %>%
  select(league, closest_match, pin_teams) %>%
  distinct(league, closest_match, .keep_all = TRUE)
team_names_map

intercept <- pull(filter(lasso_coefs, term == '(Intercept)'), estimate)
side_x <- pull(filter(lasso_coefs, term == 'side_X'), estimate)

multi_predictions <- preds %>%
  select(-periods.spreads, -teams) %>%
  pivot_longer(cols = p1:p2, names_to = 'side', values_to = 'pred') %>%
  left_join(lasso_coefs %>% select(model_id  = term, estimate), by = 'model_id') %>%
  group_by(date, league, team1, team2, mlh, mld, mla, maxbet, side) %>%
  summarise(pred = sum(pred*estimate)+intercept, .groups = 'keep') %>%
  mutate(pred = if_else(side == 'pd', pred + side_x, pred)) %>%
  pivot_wider(names_from = side, values_from = pred) %>%
  ungroup()

spreads <- preds %>%
  ungroup() %>%
  distinct(league, .keep_all = TRUE) %>%
  unnest(periods.spreads) %>%
  filter(hdp %in% c(0,0.5,-0.5)) %>%
  select(date, team1, team2, hdp, home, away)

arviot <- multi_predictions %>%
  left_join(spreads, by = join_by(date, team1, team2)) %>%
  mutate(EV1 = p1*mlh-1,
         EVD = pd*mld-1,
         EV2 = p2*mla-1) %>%
  mutate(EV1_hdp = case_when(hdp == 0 ~ p1/(1-pd)*home-1,
                             hdp == -0.5 ~ p1*home-1,
                             hdp == 0.5 ~ (p1+pd)*home-1),
         EV2_hdp = case_when(hdp == 0 ~ p2/(1-pd)*away-1,
                             hdp == -0.5 ~ (p2+pd)*away-1,
                             hdp == 0.5 ~ p2*away-1)) %>%
  group_by(team1,date) %>%
  mutate(max_hdp = pmax(EV1_hdp, EV2_hdp)) %>%
  filter((max_hdp == max(max_hdp)) %>% replace_na(TRUE)) %>%
  mutate(EV = pmax(EV1,EVD,EV2,EV1_hdp,EV2_hdp, na.rm = TRUE),
         kerroin = case_when(EV == EV1 ~ mlh,
                             EV == EVD ~ mld,
                             EV == EV2 ~ mla,
                             EV == EV1_hdp ~ home,
                             EV == EV2_hdp ~ away),
         kohde = factor(case_when(EV == EV1 | EV == EV1_hdp ~ 1,
                                  EV == EVD ~ 2,
                                  TRUE ~ 3)),
         dts = as.numeric(date-Sys.Date()),
         id = paste(date, team1, team2, sep = "-")) %>%
  ungroup %>%
  mutate_if(is.numeric, round, 3)
arviot

#tama myohemmin!
# if(arviot %>% filter(is.na(p1)) %>% nrow > 0){
#   message('ongelmia nimissä')
#   arviot <- arviot %>%
#     filter(!is.na(p1)) %>%
#     modelr::add_predictions(., qs::qread(file.path(data_path, 'clv_stack.rds')), var = 'pred') %>%
#     unnest(pred)
# } else {
#   arviot <- arviot %>%
#     modelr::add_predictions(., qs::qread(file.path(data_path, 'clv_stack.rds')), var = 'pred') %>%
#     unnest(pred)
# }

betit <- arviot %>%
  filter(EV > 0.05, dts >= 2) %>%
  filter(!(id %in% bets::hist_bets$id)) %>%
  mutate(bet = pmap_dbl(list(EV, kerroin, maxbet), bets::kelly_bet)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

if(nrow(betit) > 0){
  betit %>%
    arrange(desc(league)) %>%
    select(date:p2, EV1:EV2_hdp, hdp, kerroin, bet) %>% View

  save_bets(betit, arviot = FALSE)
}


save_bets(arviot, arviot = TRUE)

