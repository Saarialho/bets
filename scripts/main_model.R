library(bets)

league_specs <-
  tibble(league.id = c(2436, 2386, 1928, 2196, 1980, 2432, 2036, 1842, 1977, 1843, 2242, 2663, 1834, 210697),
         league = c('I1', 'P1', 'N1', 'SP1', 'E0', 'SP2', 'F1', 'D1', 'E1', 'D2', 'Liga MX', 'MLS', 'Serie A', 'Liga Profesional'))


pinnacle_odds <- get_pinnacle_odds(league_specs$league.id) %>%
  rename(league.id = leagues.id) %>%
  group_by(league.id) %>%
  nest(pelit = c(-league.id)) %>%
  left_join(league_specs)


coefs <- bets::lasso_coefs
configs <- bets::lasso_models %>%
  left_join(bets::lasso_coefs, by = c('model_id' = 'term')) %>%
  arrange(desc(estimate))


main_leagues <- bets::get_main_leagues('https://www.football-data.co.uk/mmz4281/2223/all-euro-data-2022-2023.xlsx', pinnacle_odds$league)
new_leagues <- bets::get_extra_leagues('https://www.football-data.co.uk/new/new_leagues_data.xlsx', pinnacle_odds$league)
buch_leagues <- bind_rows(main_leagues, new_leagues)

buch_leagues

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
  select(league, model_id, fit)
fits

suppressWarnings(
  predictions <- fits %>%
    left_join(pinnacle_odds) %>%
    mutate(pelit = map2(fit, pelit, possibly(predict_1x2, otherwise = NA))) %>%
    select(model_id, pelit) %>%
    unnest(pelit)
)

#jatka tasta!
predictions %>%
  select(-periods.spreads) %>%
  pivot_longer(cols = p1:p2, names_to = 'side', values_to = 'pred') %>%
  pivot_wider(names_from = model_id, values_from = pred) %>%
  mutate(side = factor(if_else(side == 'p1', '1', if_else(side == 'pd', 'X', '2')))) %>%

  select(league:side, .pred) %>%
  mutate(side = if_else(side == '1', 'p1', if_else(side == 'X', 'pd', 'p2'))) %>%
  pivot_wider(names_from = side, values_from = .pred) %>%
  filter(!is.na(league.id))










