pacman::p_load(bets, tidyverse)

log_in_pinnacle()
main_leagues_only <- T #vaihda tahan riippuen Buchdalin paivityksista
exclude_leagues <- NULL
#exclude_leagues <- c('SP1', 'SP2', 'D1')
extra_leagues <- c('Liga MX', 'MLS', 'Serie A', 'Liga Profesional')

# mallien paramterit ----
coefs <- qs::qread(here::here('models', 'lasso_coefs.rds'))
configs <- qs::qread(here::here('models', 'lasso_models.rds')) %>%
  left_join(coefs, by = c('model_id' = 'term')) %>%
  arrange(desc(estimate))

totals_coefs <- qs::qread(here::here('models', 'totals_lasso_coefs.rds'))
totals_configs <- qs::qread(here::here('models', 'totals_lasso_models.rds')) %>%
  left_join(totals_coefs, by = c('model_id' = 'term')) %>%
  arrange(desc(estimate))

# liigojen haut ----

#lisattavia I2, Belgia, F2
league_specs <-
  tibble(league.id = c(2436, 2386, 1928, 2196, 1980, 2432, 2036, 1842, 1977, 1843, 2242, 2663, 1834, 210697),
         league = c('I1', 'P1', 'N1', 'SP1', 'E0', 'SP2', 'F1', 'D1', 'E1', 'D2', 'Liga MX', 'MLS', 'Serie A', 'Liga Profesional'),
         fbref_cntry = c('ITA','POR','NED','ESP','ENG','ESP','FRA','GER','ENG','GER','MEX','USA','BRA','ARG'),
         tier = c('1st','1st','1st','1st','1st','2nd','1st','1st','2nd','2nd','1st','1st','1st','1st'))

if(!is.null(exclude_leagues)){
  league_specs <- league_specs %>%
    filter(!(league %in% exclude_leagues))
}

if(main_leagues_only){
  league_specs <- league_specs %>%
    filter(!(league %in% extra_leagues))

  pinnacle_odds <- get_pinnacle_odds(league_specs$league.id) %>%
    rename(league.id = leagues.id) %>%
    filter(date > Sys.Date()+1) %>%
    group_nest(league.id, .key = 'pelit') %>%
    left_join(league_specs)

  buch_leagues <- bets::get_main_leagues('https://www.football-data.co.uk/mmz4281/2324/all-euro-data-2023-2024.xlsx',
                                         pinnacle_odds$league)
} else {
  league_specs <- league_specs %>%
    filter(league %in% extra_leagues)

  pinnacle_odds <- get_pinnacle_odds(league_specs$league.id) %>%
    rename(league.id = leagues.id) %>%
    filter(date > Sys.Date()+1) %>%
    group_nest(league.id, .key = 'pelit') %>%
    left_join(league_specs)

  buch_leagues <- bets::get_extra_leagues('https://www.football-data.co.uk/new/new_leagues_data.xlsx',
                                          pinnacle_odds$league)
}

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
  filter(season_end_year == max(season_end_year), .by = c(competition_type, competition_name)) %>%
  arrange(competition_name, season_end_year) %>%
  slice_head(n = 1, by = country)

fbref_map <- tibble(fbref_country = pinnacle_odds$fbref_cntry,
                    tier = pinnacle_odds$tier) %>%
  left_join(latest_comps, by = c('fbref_country' = 'country')) %>%
  distinct(fbref_country, tier, season_end_year, .keep_all = TRUE)

fbref_leagues <- pmap_dfr(
  list(fbref_map$fbref_country, fbref_map$tier, fbref_map$season_end_year),
  get_fbref_data) %>%
  arrange(date) %>%
  filter(!is.na(hg))

fbref_leagues %>%
  select(league, h_xg) %>%
  group_by(league) %>%
  skimr::skim()

main_data <- join_buch_fbref(buch_data = buch_leagues, fbref_data = fbref_leagues) %>%
  select(-contains('date_'), -c(PSCH:FTR)) %>%
  filter(league %in% active_leagues) %>%
  filter(league %in% pinnacle_odds$league)

#naille ei saatu joinattua fbref dataa, nama on buch nimia
unmatched_buch <- main_data %>%
  filter(is.na(h_xg)) %>%
  #filter(!(league %in% c('D2', 'SP2'))) %>%
  select(league, home, away) %>%
  pivot_longer(-league, values_to = 'buch_name') %>%
  distinct(buch_name, .keep_all = TRUE) %>%
  arrange(buch_name) %>%
  select(league, buch_name)

target_names <- fbref_leagues %>%
  select(league, home, away) %>%
  pivot_longer(-league) %>%
  distinct(value, .keep_all = TRUE) %>%
  arrange(value) %>%
  pull(value)

unmatched_names <- unmatched_buch %>%
  mutate(closest_fbref = map_chr(buch_name, function(x) {
    teams <- target_names
    distances <- adist(x, teams)
    teams[which.min(distances)]
  }), .by = league) %>%
  filter(buch_name != closest_fbref)
unmatched_names

main_data %>%
  filter(home %in% unmatched_names$buch_name | away %in% unmatched_names$buch_name) %>%
  filter(is.na(h_xg))

main_data %>%
  filter(is.na(h_xg))

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

fbref_leagues %>%
  summarise(max_date = max(date), .by = league)

main_data %>%
  summarise(max_date = max(date), .by = league)

# muutetaan pinnacle nimet
pin_names <- main_data %>%
  replace_team_names(home, away, pattern = team_dictionary()$buch_name, replacement = team_dictionary()$pin_name)
pin_names %>% count(league)

models <- tibble(totals = c(FALSE, TRUE),
       configs = list(configs, totals_configs)) %>%
  mutate(fits = map(configs, fit_models, pin_data = pin_names, min_games = 60),
         preds = map2(fits, totals, ~pred_with_fits(.x, .y, pin_odds = pinnacle_odds)))

problems <- models %>%
  filter(totals == FALSE) %>%
  select(preds) %>%
  unnest(preds) %>%
  filter(is.na(pd))

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
    na.omit() %>%
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

betit <- data_to_save %>%
  filter(!arviot_lgl)

unmatched_names
betit %>%
  filter(totals == FALSE) %>%
  unnest(value) %>%
  select(team1:mla, EV1:EV2, hdp:away, kerroin, kohde, clv_pred, bet) %>%
  arrange(desc(clv_pred))

if(nrow(unmatched_names) == 0 & !exists('team_names_map')){
  pwalk(list(data_to_save$value, data_to_save$arviot_lgl, data_to_save$totals), save_bets)
  message('vedot lyoty')
}

#map2(betit$value, betit$totals, send_notification)

