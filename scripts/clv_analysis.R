pacman::p_load(bets, tidyverse, implied)

urls <- list(
  "https://www.football-data.co.uk/mmz4281/1718/all-euro-data-2017-2018.xlsx",
  "https://www.football-data.co.uk/mmz4281/1819/all-euro-data-2018-2019.xlsx",
  "https://www.football-data.co.uk/mmz4281/1920/all-euro-data-2019-2020.xlsx",
  "https://www.football-data.co.uk/mmz4281/2021/all-euro-data-2020-2021.xlsx",
  "https://www.football-data.co.uk/mmz4281/2122/all-euro-data-2021-2022.xlsx",
  "https://www.football-data.co.uk/mmz4281/2223/all-euro-data-2022-2023.xlsx"
)

hist_buch_data <- get_historical_buchdata(urls)

hist_bets <- qs::qread(file.path("~/Documents/bets/output", "multimodel_bets.rds")) %>%
  slice(835:n()) %>%
  replace_team_names(team1, team2, team_dictionary()$pin_name, team_dictionary()$buch_name) %>%
  mutate(date_start = date - 5,
                date_end = date + 5) %>%
  left_join(hist_buch_data %>% select(date, home:FTAG), by = dplyr::join_by(team1 == home, team2 == away, date_start <= date, date_end >= date)) %>%
  dplyr::select(-date.y, date_start, date_end) %>%
  dplyr::rename(date = date.x) %>%
  filter(date <= Sys.Date()) %>%
  select(-league.id, -altLineId, -max, -starts_with('date_'), -id)

hist_bets <- hist_bets %>%
  bind_cols(as_tibble(implied_probabilities(hist_bets %>% select(PSCH:PSCA), method = "wpo", normalize = TRUE)$probabilities) %>%
              rename(FHP = PSCH, FDP = PSCD, FAP = PSCA))

hist_bets <- hist_bets %>%
  mutate(kohde = case_when(EV == EV1 ~ 1,
                         EV == EVD ~ 2,
                         EV == EV2 ~ 3,
                         EV == EV1_hdp & hdp == 0 ~ 4,
                         EV == EV2_hdp & hdp == 0 ~ 5,
                         EV == EV1_hdp & hdp == 0.5 ~ 6, #handicap +0.5
                         EV == EV2_hdp & hdp == 0.5 ~ 7,
                         EV == EV1_hdp & hdp == -0.5 ~ 8,
                         EV == EV2_hdp & hdp == -0.5 ~ 9)) %>% # handicap -0.5
  mutate(kerroin = case_when(EV == EV1 ~ mlh,
                             EV == EVD ~ mld,
                             EV == EV2 ~ mla,
                             EV == EV1_hdp ~ home,
                             TRUE ~ away)) %>%
  #filter(!is.na(FTR)) %>%
  mutate(pnl = case_when(kohde == 1 & FTR == 'H' ~ (kerroin-1)*bet,
                         kohde == 2 & FTR == 'D' ~ (kerroin-1)*bet,
                         kohde == 3 & FTR == 'A' ~ (kerroin-1)*bet,
                         kohde == 4 & FTR == 'H' ~ (kerroin-1)*bet,
                         kohde == 5 & FTR == 'A' ~ (kerroin-1)*bet,
                         kohde == 4 & FTR == 'D' ~ 0,
                         kohde == 5 & FTR == 'D' ~ 0,
                         kohde == 6 & (FTR == 'D' | FTR == 'H') ~ (kerroin-1)*bet,
                         kohde == 7 & (FTR == 'D' | FTR == 'A') ~ (kerroin-1)*bet,
                         kohde == 8 & FTR == 'H'  ~ (kerroin-1)*bet,
                         kohde == 9 & FTR == 'A'  ~ (kerroin-1)*bet,
                         TRUE ~ -bet)) %>%
  mutate(clv = case_when(kohde == 1 ~ (kerroin/(1/FHP)-1)*bet,
                         kohde == 2 ~ (kerroin/(1/FDP)-1)*bet,
                         kohde == 3 ~ (kerroin/(1/FAP)-1)*bet,
                         kohde == 4 ~ (kerroin/((1-FDP)/(FHP))-1)*bet,
                         kohde == 5 ~ (kerroin/((1-FDP)/(FAP))-1)*bet,
                         kohde == 6 ~ (kerroin/(1/(FHP+FDP))-1)*bet,
                         kohde == 7 ~ (kerroin/(1/(FDP+FAP))-1)*bet,
                         kohde == 8 ~ (kerroin/(1/FHP)-1)*bet,
                         kohde == 9 ~ (kerroin/(1/FAP)-1)*bet)) %>%
  mutate_if(is.numeric, round, 2)

hist_bets
hist_bets %>%
  mutate(across(c('pnl', 'clv'), ~ cumsum(.))) %>%
  mutate(betno = row_number()) %>%
  select(betno, pnl, clv) %>%
  pivot_longer(-betno) %>%
  ggplot(aes(betno, value))+
  geom_line(aes(color = name))


hist_totals_data <- qs::qread(file.path("~/Documents/bets/output", "totals_bets.rds")) %>%
  replace_team_names(team1, team2, team_dictionary()$pin_name, team_dictionary()$buch_name) %>%
  mutate(date_start = date - 5,
         date_end = date + 5) %>%
  left_join(hist_buch_data %>% select(date, home:FTAG), by = dplyr::join_by(team1 == home, team2 == away, date_start <= date, date_end >= date)) %>%
  dplyr::select(-date.y, date_start, date_end) %>%
  dplyr::rename(date = date.x) %>%
  filter(date < Sys.Date()) %>%
  select(-max, -starts_with('date_'), -id) %>%
  mutate(pnl = case_when(kohde == 1 & FTHG + FTAG > 2.5 ~ bet*(over-1),
                         kohde == 2 & FTHG + FTAG < 2.5 ~ bet*(under-1),
                         TRUE ~ -bet))

hist_totals_data %>%
  mutate(across(c('pnl'), ~ cumsum(.))) %>%
  mutate(betno = row_number()) %>%
  select(betno, pnl) %>%
  pivot_longer(-betno) %>%
  ggplot(aes(betno, value))+
  geom_line(aes(color = name))



# ---- arviot ----
hist_arviot <- qs::qread(file.path("~/Documents/bets/output", "multimodel_arviot.rds")) %>%
  slice(4327:n()) %>%
  replace_team_names(team1, team2, team_dictionary()$pin_name, team_dictionary()$buch_name) %>%
  mutate(date_start = date - 5,
         date_end = date + 5) %>%
  left_join(hist_buch_data %>% select(date, home:FTAG), by = dplyr::join_by(team1 == home, team2 == away, date_start <= date, date_end >= date)) %>%
  dplyr::select(-date.y, date_start, date_end) %>%
  dplyr::rename(date = date.x) %>%
  filter(date < Sys.Date()) %>%
  select(-league.id, -altLineId, -max, -starts_with('date_'), -id)

hist_arviot <- hist_arviot %>%
  bind_cols(as_tibble(implied_probabilities(hist_arviot %>% select(PSCH:PSCA), method = "wpo", normalize = TRUE)$probabilities) %>%
              rename(FHP = PSCH, FDP = PSCD, FAP = PSCA))

hist_arviot <- hist_arviot %>%
  mutate(clv_home = mlh/(1/FHP)-1,
         clv_draw = mld/(1/FDP)-1,
         clv_away = mla/(1/FAP)-1) %>%
  select(league, date, team1, team2, mlh:mla, maxbet, EV1:EV2, contains('clv_')) %>%
  pivot_longer(contains('EV'), values_to = 'EV') %>%
  mutate(clv = case_when(name == 'EV1' ~ clv_home,
                         name == 'EVD' ~ clv_draw,
                         TRUE ~ clv_away),
         kerroin = case_when(name == 'EV1' ~ mlh,
                         name == 'EVD' ~ mld,
                         TRUE ~ mla),
         kohde = case_when(name == 'EV1' ~ 1,
                           name == 'EVD' ~ 2,
                           TRUE ~ 3)) %>%
  mutate(kerroin = log(kerroin),
         maxbet = log(maxbet),
         kohde = factor(kohde))

hist_arviot

clv_reg <- lm(clv ~ EV + kerroin + league + kohde, data = hist_arviot)
clv_reg %>% summary()
qs::qsave(clv_reg, file = here::here('output', 'clv_reg.rds'))

hist_arviot
  #ggstatsplot::grouped_ggscatterstats(x = ev, y = clv, grouping.var = name) %>%
  ggstatsplot::ggscatterstats(x = EV, y = clv)


