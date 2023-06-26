library(tidymodels)
library(glmnet)

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
  filter(n > 500)

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
  step_novel(all_nominal_predictors(), new_level='Unseen') %>% # deal with new data being present in new data
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

wf <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(model_spec)
wf

options(tidymodels.dark = TRUE)

tuned <- wf %>%
  tune_grid(
    resamples = rsample::bootstraps(oof_predictions, 50),
    grid = 50,
    metrics = metric_set(rsq_trad),
    control = control_grid(save_pred = FALSE, save_workflow = FALSE)
  )
tuned

best_param <- tune::select_best(tuned, metric = 'rsq_trad')

meta_model <- wf %>%
  finalize_workflow(select_best(tuned, metric = 'rsq_trad')) %>%
  parsnip::fit(data = oof_predictions) %>%
  butcher::butcher()

use_data(meta_model, overwrite = TRUE)

lasso_coefs <- meta_model %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0)

use_data(lasso_coefs, overwrite = TRUE)

lasso_coefs

selected_models <- lasso_coefs %>%
  filter(term != '(Intercept)') %>%
  pull(term)

meta_model_preds <- oof_predictions %>%
  bind_cols(predict(meta_model, .))

meta_model_preds %>%
  select(target, .pred) %>%
  ggstatsplot::ggscatterstats(x = .pred, y = target)

meta_model_preds %>%
  select(where(is.numeric)) %>%
  pivot_longer(-target) %>%
  group_by(name) %>%
  summarise(cor = cor(target, value)) %>%
  arrange(desc(cor))

preds






