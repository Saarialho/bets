pacman::p_load(bets, tidyverse, implied)

urls <-
  tibble::tribble(
  ~season, ~filename,
  '2324', 'https://www.football-data.co.uk/mmz4281/2324/all-euro-data-2023-2024.xlsx'
  )

hist_buch_data <- get_historical_buchdata(urls)

hist_bets <- qs::qread(file.path("~/Documents/bets/output", "multimodel_bets.rds")) %>%
  replace_team_names(team1, team2, team_dictionary()$pin_name, team_dictionary()$buch_name) %>%
  mutate(date_start = date - 5,
                date_end = date + 5) %>%
  left_join(hist_buch_data %>% select(date, home:FTAG),
            by = dplyr::join_by(team1 == home, team2 == away, date_start <= date, date_end >= date)) %>%
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
  filter(!is.na(FTR)) %>%
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
  mutate(clv_raw = case_when(kohde == 1 ~ (kerroin/(1/FHP)-1)*1,
                         kohde == 2 ~ (kerroin/(1/FDP)-1)*1,
                         kohde == 3 ~ (kerroin/(1/FAP)-1)*1,
                         kohde == 4 ~ (kerroin/((1-FDP)/(FHP))-1)*1,
                         kohde == 5 ~ (kerroin/((1-FDP)/(FAP))-1)*1,
                         kohde == 6 ~ (kerroin/(1/(FHP+FDP))-1)*1,
                         kohde == 7 ~ (kerroin/(1/(FDP+FAP))-1)*1,
                         kohde == 8 ~ (kerroin/(1/FHP)-1)*1,
                         kohde == 9 ~ (kerroin/(1/FAP)-1)*1)) %>%
  mutate_if(is.numeric, round, 2)


hist_bets %>%
  mutate(across(c('pnl', 'clv'), ~ cumsum(.))) %>%
  mutate(betno = row_number()) %>%
  select(betno, pnl, clv) %>%
  pivot_longer(-betno) %>%
  ggplot(aes(betno, value))+
  geom_line(aes(color = name))

hist_bets %>% ggstatsplot::ggscatterstats(EV, clv_raw)
summary(lm(clv_raw ~ EV + kohde + kerroin, data = hist_bets))

hist_bets %>%
  select(EV, clv_raw) %>%
  mutate(bin = ntile(EV, 3)) %>%
  summarise(mean_ev = mean(EV),
            mean_clv = mean(clv_raw),
            n = n(),
            .by = bin) %>%
  arrange(bin)

# ---- arviot ----
hist_arviot <- qs::qread(file.path("~/Documents/bets/output", "multimodel_arviot.rds")) %>%
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
         moneyline = case_when(name == 'EV1' ~ mlh,
                         name == 'EVD' ~ mld,
                         TRUE ~ mla),
         kohde = case_when(name == 'EV1' ~ 1,
                           name == 'EVD' ~ 2,
                           TRUE ~ 3)) %>%
  mutate(moneyline = log(moneyline),
         maxbet = log(maxbet),
         kohde = factor(kohde))

hist_arviot <- hist_arviot %>%
  mutate(clv = datawizard::winsorize(clv, threshold = 0.01),
         EV = datawizard::winsorize(EV, threshold = 0.01))

#lisaa liiga dummyt jossain vaiheessa
clv_reg <- lm(clv ~ EV + kohde + moneyline, data = hist_arviot)
clv_reg %>% summary()
qs::qsave(clv_reg, file = here::here('output', 'clv_reg.rds'))

hist_arviot %>%
  #ggstatsplot::grouped_ggscatterstats(x = EV, y = clv, grouping.var = name) %>%
  ggstatsplot::ggscatterstats(x = EV, y = clv)

hist_arviot %>%
  filter(EV > 0) %>%
  ggstatsplot::ggscatterstats(x = EV, y = clv)


# hist_arviot %>%
#   select(EV, clv) %>%
#   mutate(ev_bin = ntile(EV, 20)) %>%
#   summarise(EV = mean(EV, na.rm = TRUE),
#             clv = mean(clv, na.rm = TRUE),
#             .by = ev_bin) %>%
#   ggstatsplot::ggscatterstats(x = EV, y = clv)
#
#
# library(tidymodels)
#
# train_data <- hist_arviot %>%
#   select(league, kohde, kerroin, EV, clv)
#
# model_spec <-
#   linear_reg(penalty = tune::tune(), mixture = 1) %>%
#   set_engine("glmnet", lower.limits = 0, lambda.min.ratio = 0)
#
# model_rec <- recipe(train_data %>% dplyr::slice(0)) %>%
#   update_role(everything()) %>%
#   update_role(clv, new_role = "outcome") %>%
#   step_novel(all_nominal_predictors(), new_level='Unseen') %>%
#   step_dummy(all_nominal_predictors()) %>%
#   step_zv(all_numeric_predictors())
#
# discrete_rec <- recipe(train_data %>% dplyr::slice(0)) %>%
#   update_role(everything()) %>%
#   update_role(clv, new_role = "outcome") %>%
#   step_discretize(EV, num_breaks = 9) %>%
#   step_novel(all_nominal_predictors(), new_level='Unseen') %>%
#   step_dummy(all_nominal_predictors()) %>%
#   step_zv(all_numeric_predictors())
#
# interact_rec <-
#   recipe(train_data %>% dplyr::slice(0)) %>%
#   update_role(everything()) %>%
#   update_role(clv, new_role = "outcome") %>%
#   step_novel(all_nominal_predictors(), new_level='Unseen') %>%
#   step_interact(terms = ~ kohde:league) %>%
#   step_dummy(all_nominal_predictors()) %>%
#   step_zv(all_numeric_predictors())
#
# wfset <-
#   workflow_set(
#     preproc = list(norm = model_rec, interact = interact_rec, discrete = discrete_rec),
#     models = list(lasso = model_spec)
#   )
# wfset
#
# options(tidymodels.dark = TRUE)
#
# tuned <-
#   wfset %>%
#   workflow_map(
#     resamples = rsample::bootstraps(train_data, 20),
#     grid = 20,
#     metrics = metric_set(rsq_trad),
#     control = control_grid(save_pred = FALSE, save_workflow = TRUE)
#   )
#
# tuned %>%
#   rank_results()
#
#
#
# ei_loytyneet <- hist_arviot %>%
#   filter(is.na(PSCH)) %>%
#   select(team1, team2, home) %>%
#   pivot_longer(-home) %>%
#   distinct(value)
#
# ei_loytyneet %>%
#   mutate(in_buch = value %in% hist_buch_data$home) %>%
#   filter(!in_buch)
