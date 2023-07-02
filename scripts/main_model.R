pacman::p_load(bets, tidyverse)

log_in_pinnacle()

# mallien paramterit ----
coefs <- bets::lasso_coefs
configs <- bets::lasso_models %>%
  left_join(coefs, by = c('model_id' = 'term')) %>%
  arrange(desc(estimate))

totals_coefs <- bets::totals_lasso_coefs
totals_configs <- bets::totals_lasso_models %>%
  left_join(totals_coefs, by = c('model_id' = 'term')) %>%
  arrange(desc(estimate))

# liigojen haut ----

league_specs <-
  tibble(league.id = c(2436, 2386, 1928, 2196, 1980, 2432, 2036, 1842, 1977, 1843, 2242, 2663, 1834, 210697),
         league = c('I1', 'P1', 'N1', 'SP1', 'E0', 'SP2', 'F1', 'D1', 'E1', 'D2', 'Liga MX', 'MLS', 'Serie A', 'Liga Profesional'),
         fbref_cntry = c('ITA','POR','NED','ESP','ENG','ESP','FRA','GER','ENG','GER','MEX','USA','BRA','ARG'),
         tier = c('1st','1st','1st','1st','1st','2nd','1st','1st','2nd','2nd','1st','1st','1st','1st'))

pinnacle_odds <- get_pinnacle_odds(league_specs$league.id) %>%
  rename(league.id = leagues.id) %>%
  group_nest(league.id, .key = 'pelit') %>%
  left_join(league_specs)

main_leagues <- bets::get_main_leagues('https://www.football-data.co.uk/mmz4281/2223/all-euro-data-2022-2023.xlsx',
                                       pinnacle_odds$league)
new_leagues <- bets::get_extra_leagues('https://www.football-data.co.uk/new/new_leagues_data.xlsx',
                                       pinnacle_odds$league)

buch_leagues <- bind_rows(main_leagues, new_leagues)

active_leagues <- buch_leagues %>%
  summarise(days_since_last = as.double(Sys.Date()-last(date)), .by = league) %>%
  filter(days_since_last < 20) %>%
  pull(league)

pinnacle_odds <- pinnacle_odds %>%
  filter(league %in% active_leagues)

latest_comps <- vroom::vroom(
  'https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv',
  show_col_types = FALSE) %>%
  filter(country %in% unique(pinnacle_odds$fbref_cntry), gender == 'M') %>%
  select(competition_type, competition_name, country, last_season, season_end_year) %>%
  distinct() %>%
  filter(str_detect(competition_type, '1st|2nd')) %>%
  filter(season_end_year == max(season_end_year), .by = c(competition_type, competition_name))

fbref_leagues <- map2_dfr(pinnacle_odds$fbref_cntry, pinnacle_odds$tier, get_fbref_data, seasons = 2023) %>%
  arrange(date) %>%
  filter(!is.na(hg))

fbref_leagues %>%
  summarise(max_date = max(date), .by = league)

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

models <- tibble(totals = c(FALSE, TRUE),
       configs = list(configs, totals_configs)) %>%
  mutate(fits = map(configs, fit_models, pin_data = pin_names),
         preds = map2(fits, totals, ~pred_with_fits(.x, .y, pin_odds = pinnacle_odds)))

problems <- models %>%
  select(preds) %>%
  slice(2) %>%
  unnest(preds) %>%
  filter(is.na(prob_under))

if(nrow(problems) > 0){
  team_names_map <- problems %>%
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
    mutate(closest_match = map_chr(pin_teams, function(x) {
      teams <- data[[1]]$teams
      distances <- adist(x, teams)
      teams[which.min(distances)]
    }), .by = league) %>%
    select(league, closest_match, pin_teams) %>%
    distinct(league, closest_match, .keep_all = TRUE)
  team_names_map

}

models <- models %>%
  mutate(arviot = map2(preds, totals, aggregate_predictions)) %>%
  mutate(betit = map2(arviot, totals, find_bets))

data_to_save <- models %>%
  select(totals, arviot, betit) %>%
  pivot_longer(-totals) %>%
  mutate(arviot_lgl = if_else(name == 'arviot', TRUE, FALSE))

pwalk(list(data_to_save$value, data_to_save$arviot_lgl, data_to_save$totals), save_bets)

betit <- data_to_save %>%
  filter(!arviot_lgl)

betit$value %>% map(send_notification)

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

