pacman::p_load(bets, tidyverse, implied, tidymodels)

urls <-
  tibble::tribble(
  ~season, ~filename,
  '2324', 'https://www.football-data.co.uk/mmz4281/2324/all-euro-data-2023-2024.xlsx'
  )

hist_buch_data <- get_historical_buchdata(urls)

hist_bets <- qs::qread(file.path("~/Documents/bets/output", "multimodel_bets.rds")) %>%
  replace_team_names(team1, team2, 'Vasco da Gama', 'Vasco Da Gama') %>%
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
                         EV == EV2_hdp & hdp == -0.5 ~ 9)) %>% #handicap -0.5
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
                         kohde == 6 & (FTR == 'D' | FTR == 'H') ~ (kerroin-1)*bet, #kotijengi +0.5 ja betsi kotijengille
                         kohde == 7 & FTR == 'A' ~ (kerroin-1)*bet,
                         kohde == 8 & FTR == 'H'  ~ (kerroin-1)*bet,
                         kohde == 9 & (FTR == 'A' | FTR == 'D')  ~ (kerroin-1)*bet, #kotijengi -0.5 ja betsi vieraallle
                         TRUE ~ -bet)) %>%
  mutate(clv_raw = case_when(kohde == 1 ~ (kerroin/(1/FHP)-1),
                             kohde == 2 ~ (kerroin/(1/FDP)-1),
                             kohde == 3 ~ (kerroin/(1/FAP)-1),
                             kohde == 4 ~ (kerroin/((1-FDP)/(FHP))-1),
                             kohde == 5 ~ (kerroin/((1-FDP)/(FAP))-1),
                             kohde == 6 ~ (kerroin/(1/(FHP+FDP))-1),
                             kohde == 7 ~ (kerroin/(1/FAP)-1),
                             kohde == 8 ~ (kerroin/(1/FHP)-1),
                             kohde == 9 ~ (kerroin/(1/(FAP+FDP))-1))) %>%
  mutate(clv = clv_raw*bet) %>%
  mutate(bet_no = row_number(),
         cum_clv = cumsum(clv))

hist_bets %>%
  filter(n() > 20, .by = league) %>%
  mutate(bet_no = row_number(), .by = league) %>%
  mutate(cum_clv = cumsum(clv),
         .by = league) %>%
  ggplot(aes(bet_no, cum_clv, color = league))+
  geom_line()

hist_bets %>%
  select(bet_no, clv, clv_raw, pnl) %>%
  pivot_longer(-bet_no) %>%
  mutate(value = value/sd(value), .by = name) %>%
  mutate(rmean = roll::roll_mean(value, 20), .by = name) %>%
  ggplot(aes(bet_no, rmean, color = name))+
  geom_line()

hist_bets %>%
  summarise(clv_raw = sum(clv),
            n = n(),
            .by = league) %>%
  arrange(desc(n))

hist_bets %>% ggstatsplot::ggscatterstats(EV, clv_raw)
summary(lm(clv_raw ~ EV + kohde + kerroin, data = hist_bets))
summary(lm(clv_raw ~ clv_pred, data = hist_bets))
summary(lm(clv_raw ~ EV, data = hist_bets))

hist_bets %>%
  ggstatsplot::ggscatterstats(x = clv_pred, y = clv_raw)

hist_bets %>%
  ggstatsplot::ggscatterstats(x = clv_pred, y = EV)

hist_bets %>%
  select(clv_pred, clv_raw, EV) %>%
  mutate(bet_no = row_number()) %>%
  pivot_longer(cols = c(clv_pred, EV)) %>%
  mutate(cor = roll::roll_cor(value, clv_raw, width = 50), .by = name) %>%
  na.omit() %>%
  ggplot(aes(bet_no, cor, color = name))+
  geom_line()+
  geom_smooth(method = 'lm')

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
  replace_team_names(team1, team2, 'Vasco da Gama', 'Vasco Da Gama') %>%
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
  select(league, date, team1, team2, mlh:mla, maxbet, dts, EV1:EV2, contains('clv_')) %>%
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
  mutate(kohde = factor(kohde),
         plus_ev = factor(if_else(EV >= 0, 1, 0)),
         EV_sqrd = EV^2)

#TSEK NAMA NIMET
hist_arviot %>%
  filter(is.na(clv)) %>%
  select(team1, team2) %>%
  pivot_longer(everything()) %>%
  count(value)

hist_arviot <- hist_arviot %>%
  filter(!is.na(EV))

# hist_arviot <- hist_arviot %>%
#   mutate(clv = datawizard::winsorize(clv, threshold = 0.005),
#          EV = datawizard::winsorize(EV, threshold = 0.005))

hist_bets %>%
  mutate(across(c('pnl', 'clv'), ~ cumsum(.))) %>%
  mutate(betno = row_number()) %>%
  select(betno, pnl, clv) %>%
  pivot_longer(-betno) %>%
  ggplot(aes(betno, value))+
  geom_line(aes(color = name))

hist_bets %>%
  #slice_tail(n = nrow(.)%/%2) %>%
  summarise(clv = sum(clv)/sum(bet),
            roi = sum(pnl)/sum(bet))

#lisaa liiga dummyt jossain vaiheessa
clv_reg <- lm(clv ~ EV + kohde + moneyline + plus_ev:EV + moneyline:EV + EV:league + EV:maxbet, data = hist_arviot)
clv_reg %>% summary()
qs::qsave(clv_reg, file = here::here('output', 'clv_reg.rds'))

model_df <- hist_arviot %>%
  select(league, kohde, EV, clv, plus_ev, maxbet, league, moneyline)

all_rec <-
  recipe(model_df %>% dplyr::slice(0)) %>%
  update_role(everything()) %>%
  update_role(clv, new_role = "outcome") %>%
  step_novel(all_nominal_predictors(), new_level='Unseen') %>%
  step_normalize(all_numeric_predictors()) %>%
  step_interact(terms = ~ EV:maxbet) %>%
  step_interact(terms = ~ EV:moneyline) %>%
  step_interact(terms = ~ EV:plus_ev) %>%
  step_interact(terms = ~ EV:league) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

model_spec <-
  linear_reg() %>%
  set_engine("lm")

clv_workflow <-
  workflow() %>%
  add_recipe(all_rec) %>%
  add_model(model_spec)

clv_fit <- fit(clv_workflow, hist_arviot)

clv_fit$fit$fit$fit %>% summary()
clv_reg %>% summary()


clv_fit %>%
  tidy() %>%
  left_join(clv_reg %>%
              tidy() %>%
              select(term, clv_reg = estimate)) %>%
  mutate(abs_tstat = abs(statistic)) %>%
  arrange(desc(abs_tstat)) %>%
  na.omit()


clv_fit <- clv_fit %>%
  butcher::butcher()

qs::qsave(clv_fit, file = here::here('output', 'clv_fit.rds'))

clv_reg %>% performance::check_autocorrelation()
clv_reg %>% performance::check_collinearity()
clv_reg %>% performance::check_heteroscedasticity()
clv_reg %>% performance::check_predictions()
#ei pidakkaan olla kun mean != 0?
clv_reg %>% performance::check_normality()
#clv_reg %>% performance::check_model()

summary(lm(clv ~ EV + kohde + moneyline, data = hist_arviot))
summary(lm(clv ~ EV + kohde + moneyline + plus_ev:EV, data = hist_arviot))
summary(lm(clv ~ EV + kohde + moneyline + plus_ev:EV + moneyline:EV, data = hist_arviot))
summary(lm(clv ~ EV + kohde + moneyline + plus_ev:EV + EV:league, data = hist_arviot))
summary(lm(clv ~ EV + kohde + moneyline + plus_ev:EV + moneyline:EV, data = hist_arviot))
summary(lm(clv ~ EV + kohde + moneyline + plus_ev:EV + moneyline:EV + EV:league, data = hist_arviot))
summary(lm(clv ~ EV + kohde + moneyline + plus_ev:EV + moneyline:EV + EV:league + EV:maxbet, data = hist_arviot))

hist_arviot %>%
  mutate(EV = datawizard::winsorize(EV, 0.0025)) %>%
  ggplot(aes(EV, clv)) +
  geom_point(alpha = 1 / 10, cex = 1) +
  labs(y = "Closing line value", x = "Expected value") +
  geom_smooth(se = FALSE, col = "red")+
  geom_smooth(se = FALSE, col = "blue", method = 'lm')

hist_arviot %>%
  #ggstatsplot::grouped_ggscatterstats(x = EV, y = clv, grouping.var = name) %>%
  ggstatsplot::ggscatterstats(x = EV, y = clv)

hist_arviot %>%
  filter(EV > 0) %>%
  ggstatsplot::ggscatterstats(x = EV, y = clv)

hist_bets %>%
  select(clv_pred, clv_raw) %>%
  na.omit() %>%
  ggstatsplot::ggscatterstats(x = clv_pred, y = clv_raw)

# hist_arviot %>%
#   select(EV, clv) %>%
#   mutate(ev_bin = ntile(EV, 20)) %>%
#   summarise(EV = mean(EV, na.rm = TRUE),
#             clv = mean(clv, na.rm = TRUE),
#             .by = ev_bin) %>%
#   ggstatsplot::ggscatterstats(x = EV, y = clv)
#
#

train_data <- hist_arviot %>%
  select(league, kohde, EV, clv, plus_ev, maxbet, league, moneyline) %>%
  na.omit()

model_spec <-
  linear_reg() %>%
  set_engine("lm")

base_rec <- recipe(train_data %>% dplyr::slice(0)) %>%
  update_role(everything()) %>%
  update_role(clv, new_role = "outcome") %>%
  step_novel(all_nominal_predictors(), new_level='Unseen')

model_rec <- base_rec %>%
  step_log(moneyline, maxbet) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

league_rec <- base_rec %>%
  step_log(moneyline, maxbet) %>%
  step_interact(terms = ~ EV:league) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

plus_rec <-
  base_rec %>%
  step_log(moneyline, maxbet) %>%
  step_interact(terms = ~ plus_ev:EV) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

maxbet_rec <-
  base_rec %>%
  step_log(moneyline, maxbet) %>%
  step_interact(terms = ~ EV:maxbet) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

all_rec <-
  base_rec %>%
  step_log(moneyline, maxbet) %>%
  step_interact(terms = ~ EV:maxbet) %>%
  step_interact(terms = ~ EV:plus_ev) %>%
  step_interact(terms = ~ EV:league) %>%
  step_interact(terms = ~ EV:moneyline) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

all_norm <-
  base_rec %>%
  step_normalize(all_numeric_predictors()) %>%
  step_interact(terms = ~ EV:maxbet) %>%
  step_interact(terms = ~ EV:plus_ev) %>%
  step_interact(terms = ~ EV:league) %>%
  step_interact(terms = ~ EV:moneyline) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())


all_norm_spline <-
  base_rec %>%
  step_interact(terms = ~ EV:maxbet) %>%
  step_interact(terms = ~ EV:plus_ev) %>%
  step_interact(terms = ~ EV:league) %>%
  step_interact(terms = ~ EV:moneyline) %>%
  step_spline_natural(EV, deg_free = 10) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

wfset <-
  workflow_set(
    preproc = list(norm = model_rec, league = league_rec, maxbet = maxbet_rec, plus = plus_rec,
                   all = all_rec, spline = all_norm_spline, all_norm = all_norm),
    models = list(lm = model_spec)
  )
wfset

options(tidymodels.dark = TRUE)

tuned <-
  wfset %>%
  workflow_map(
    resamples = vfold_cv(train_data, v = 10, repeats = 2, strata = clv),
    #grid = 20,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, save_workflow = TRUE)
  )

tuned %>%
  rank_results(select_best = TRUE)

tuned %>%
  extract_workflow('all_norm_lm')

valid_preds <- tuned %>%
  collect_predictions() %>%
  filter(wflow_id == 'all_norm_lm')

valid_preds %>%
  ggplot(aes(clv, .pred)) +
  geom_abline(col = "green", lty = 2) +
  geom_point(alpha = 0.4, cex = 1) +
  geom_smooth(se = FALSE, col = "red") +
  geom_smooth(se = FALSE, col = "blue", method = 'lm') +
  coord_obs_pred() +
  labs(x = "Predicted clv", y = "Observed clv")

lin_reg_fit <- fit_best(tuned)
lin_reg_coef <- tidy(lin_reg_fit)
lin_reg_metrics <- collect_metrics(tuned)

lm_cal <- cal_estimate_linear(valid_preds, truth = clv)

valid_preds %>%
  cal_apply(lm_cal) %>%
  ggplot(aes(clv, .pred)) +
  geom_abline(col = "green", lty = 2) +
  geom_point(alpha = 0.4, cex = 1) +
  geom_smooth(se = FALSE, col = "red") +
  geom_smooth(se = FALSE, col = "blue", method = 'lm') +
  coord_obs_pred() +
  labs(x = "Predicted clv", y = "Observed clv")

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




# hist_arviot %>%
#   select(EV, clv, moneyline) %>%
#   mutate(bin = ntile(moneyline, 3)) %>%
#   mutate(bin = factor(bin)) %>%
#   ggpubr::ggscatter(x = "EV", y = "clv", color = "bin",
#           palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           add = "reg.line")
#
# hist_arviot %>%
#   select(EV, clv, kohde) %>%
#   ggpubr::ggscatter(x = "EV", y = "clv", color = "kohde",
#                     palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#                     add = "reg.line")
