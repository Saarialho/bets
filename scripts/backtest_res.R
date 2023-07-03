library(tidymodels)
library(glmnet)
library(implied)

#kopioi workflowsetsistä totals interaktiot, esim ristin todari voi vaihdella aika paljon per liiga
data <- bets::hist_buch_data

data %>%
  bind_cols(as_tibble(implied_probabilities(data %>% select(PSCH:PSCA), method = "wpo", normalize = TRUE)$probabilities) %>%
              rename(FHP = PSCH, FDP = PSCD, FAP = PSCA)) %>%
  ggstatsplot::ggbetweenstats(league, FDP)

preds <- here::here('output') %>%
  fs::dir_ls() %>%
  map_dfr(qs::qread) %>%
  unnest(c(data)) %>%
  arrange(date) %>%
  filter(!is.na(p1))
preds

rps_scores <- preds %>%
  group_nest(wmkt, wxg, wgoals, xi) %>%
  mutate(rps = map(data, ~goalmodel::score_predictions(predictions = matrix(c(.$p1, .$pd, .$p2), ncol = 3),
                                                       observed = .$obs,
                                                       score = 'rps')$rps)) %>%
  select(-data) %>%
  mutate(rps = map_dbl(rps, ~mean(., na.rm = TRUE))) %>%
  arrange(rps)
rps_scores

game_ids <- preds %>%
  group_by(date, home, away) %>%
  count() %>%
  ungroup() %>%
  mutate(game_id = row_number())

problem_games <- game_ids %>%
  filter(n != 1000)

oof_predictions <- preds %>%
  left_join(game_ids) %>%
  filter(!(game_id %in% problem_games$game_id)) %>%
  mutate(across(c(wmkt:xi), ~round(., 5))) %>%
  mutate(model_id = glue::glue('model_{xi}_{wmkt}')) %>%
  select(game_id, model_id, league, FHP:FAP, p1:p2) %>%
  pivot_longer(FHP:FAP, names_to = 'close', values_to = 'target') %>%
  mutate(pred = case_when(close == 'FHP' ~ p1,
                          close == 'FDP' ~ pd,
                          TRUE ~ p2),
         side = factor(case_when(close == 'FHP' ~ '1',
                                 close == 'FDP' ~ 'X',
                                 TRUE ~ '2'))) %>%
  mutate(league = factor(league)) %>%
  mutate(model_id = factor(model_id)) %>%
  select(-close, -p1, -pd, -p2) %>%
  pivot_wider(names_from = model_id, values_from = pred) %>%
  select(-game_id)

oof_predictions

model_spec <-
  linear_reg(penalty = tune::tune(), mixture = 1) %>%
  set_engine("glmnet", lower.limits = 0, lambda.min.ratio = 0)

model_rec <- recipe(oof_predictions %>% dplyr::slice(0)) %>%
  update_role(everything()) %>%
  update_role(target, new_role = "outcome") %>%
  step_novel(all_nominal_predictors(), new_level='Unseen') %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

interact_rec <-
  recipe(oof_predictions %>% dplyr::slice(0)) %>%
  update_role(everything()) %>%
  update_role(target, new_role = "outcome") %>%
  step_novel(all_nominal_predictors(), new_level='Unseen') %>%
  step_interact(terms = ~ side:league) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

wfset <-
  workflow_set(
    preproc = list(norm = model_rec, interact = interact_rec),
    models = list(lasso = model_spec)
  )
wfset

options(tidymodels.dark = TRUE)

tuned <-
  wfset %>%
  workflow_map(
    resamples = rsample::bootstraps(oof_predictions, 80),
    grid = 80,
    metrics = metric_set(rsq_trad),
    control = control_grid(save_pred = FALSE, save_workflow = TRUE)
  )

tuned %>%
  rank_results()

best_param <-
  tuned %>%
  extract_workflow_set_result("interact_lasso") %>%
  select_best(metric = "rsq_trad")

meta_model <- tuned %>%
  extract_workflow("interact_lasso") %>%
  finalize_workflow(best_param) %>%
  parsnip::fit(data = oof_predictions) %>%
  butcher::butcher()

lasso_coefs <- meta_model %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0)

lasso_coefs %>%
  qs::qsave(here::here('models', 'lasso_coefs.rds'))

lasso_coefs

selected_models <- lasso_coefs %>%
  filter(term != '(Intercept)') %>%
  pull(term)

meta_model_preds <- oof_predictions %>%
  bind_cols(predict(meta_model, .))

meta_model_preds %>%
  select(where(is.numeric)) %>%
  pivot_longer(-target) %>%
  group_by(name) %>%
  summarise(cor = cor(target, value)) %>%
  arrange(desc(cor))

meta_model_preds %>%
  select(target, .pred) %>%
  ggstatsplot::ggscatterstats(x = .pred, y = target)


#nain saa manuaalisesti tehtya!
intercept <- pull(filter(lasso_coefs, term == '(Intercept)'), estimate)

meta_model_preds %>%
  select(league, target, side, .pred, any_of(selected_models[-length(selected_models)])) %>%
  pivot_longer(contains('model')) %>%
  mutate(league_interaction = glue::glue('side{side}_x_league{league}')) %>%
  mutate(league_dummy = glue::glue('league_{league}')) %>%
  mutate(side_dummy = glue::glue('side_{side}')) %>%
  left_join(lasso_coefs %>% select(name = term, estimate)) %>%
  left_join(lasso_coefs %>% select(name = term, int_coef = estimate), by = c('league_interaction' = 'name')) %>%
  left_join(lasso_coefs %>% select(name = term, dummy_coef = estimate), by = c('league_dummy' = 'name')) %>%
  left_join(lasso_coefs %>% select(name = term, side_coef = estimate), by = c('side_dummy' = 'name')) %>%
  mutate(across(contains('_coef'), ~replace_na(., 0))) %>%
  group_by(target, side, .pred, int_coef, dummy_coef, side_coef) %>%
  summarise(man_pred = sum(value*estimate)+intercept) %>%
  mutate(man_pred = man_pred + int_coef + dummy_coef + side_coef) %>%
  filter(abs(man_pred-.pred) > 0.0001)

meta_model_preds %>%
  select(target, side, .pred, any_of(selected_models[-length(selected_models)])) %>%
  skimr::skim()

lasso_models <- preds %>%
  distinct(wmkt, wxg, wgoals, xi) %>%
  mutate(across(c(wmkt:xi), ~round(., 5))) %>%
  mutate(model_id = glue::glue('model_{xi}_{wmkt}')) %>%
  filter(model_id %in% lasso_coefs$term)

lasso_models %>%
  qs::qsave(here::here('models', 'lasso_models.rds'))


