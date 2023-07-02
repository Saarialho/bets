library(tidymodels)
library(glmnet)

#totalsissa vois olla jarkeva olla league dummyt*side?
bets::hist_totals_data %>%
  mutate(total = FTHG+FTAG) %>%
  select(league, total) %>%
  ggstatsplot::ggbetweenstats(league, total)

preds <- here::here('output') %>%
  fs::dir_ls() %>%
  keep(., stringr::str_detect(., 'total')) %>%
  keep(., !stringr::str_detect(., '_arviot')) %>%
  keep(., !stringr::str_detect(., '_bets')) %>%
  tidytable::map_dfr(qs::qread) %>%
  tidytable::unnest(c(data)) %>%
  tidytable::arrange(date) %>%
  tidytable::filter(!is.na(prob_under))
preds

game_ids <- preds %>%
  group_by(date, home, away) %>%
  count() %>%
  ungroup() %>%
  mutate(game_id = row_number())

problem_games <- game_ids %>%
  filter(n != 750)

oof_predictions <- preds %>%
  tidytable::left_join(game_ids) %>%
  tidytable::filter(!(game_id %in% problem_games$game_id)) %>%
  tidytable::mutate(tidytable::across(c(wmkt:xi), ~round(., 5))) %>%
  tidytable::mutate(model_id = glue::glue('model_{xi}_{wmkt}')) %>%
  tidytable::select(date, game_id, model_id, league, fo2.5:fu2.5, prob_under:prob_over) %>%
  tidytable::pivot_longer(fo2.5:fu2.5, names_to = 'close', values_to = 'target') %>%
  tidytable::mutate(pred = case_when(close == 'fo2.5' ~ prob_over,
                          TRUE ~ prob_under),
         side = factor(case_when(close == 'fo2.5' ~ '1',
                                 TRUE ~ '2'))) %>%
  tidytable::mutate(league = factor(league)) %>%
  tidytable::mutate(model_id = factor(model_id)) %>%
  tidytable::select(-close, -prob_under, -prob_over) %>%
  tidytable::pivot_wider(names_from = model_id, values_from = pred) %>%
  tidytable::select(-game_id)
oof_predictions

model_spec <-
  linear_reg(penalty = tune::tune(), mixture = 1) %>%
  set_engine("glmnet", lower.limits = 0, lambda.min.ratio = 0)

model_rec <- recipe(oof_predictions %>% dplyr::slice(0)) %>%
  update_role(everything()) %>%
  update_role(target, new_role = "outcome") %>%
  update_role(date, new_role = "date") %>%
  step_novel(all_nominal_predictors(), new_level='Unseen') %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors())

interact_rec <-
  recipe(oof_predictions %>% dplyr::slice(0)) %>%
  update_role(everything()) %>%
  update_role(target, new_role = "outcome") %>%
  update_role(date, new_role = "date") %>%
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
    resamples = rsample::bootstraps(oof_predictions, 50),
    grid = 50,
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

totals_lasso_coefs <- meta_model %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0)

use_data(totals_lasso_coefs, overwrite = TRUE)

selected_models <- totals_lasso_coefs %>%
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

# meta_model_preds %>%
#   select(target, .pred) %>%
#   ggstatsplot::ggscatterstats(x = .pred, y = target)

#nain saa manuaalisesti tehtya!
#pitaa paiivittaa interaktiot!
intercept <- pull(filter(totals_lasso_coefs, term == '(Intercept)'), estimate)
side_x <- pull(filter(totals_lasso_coefs, term == 'side_X2'), estimate)


meta_model_preds %>%
  select(league, target, side, .pred, any_of(selected_models[-length(selected_models)])) %>%
  pivot_longer(contains('model')) %>%
  mutate(league_interaction = glue::glue('side{side}_x_league{league}')) %>%
  left_join(totals_lasso_coefs %>% select(name = term, estimate)) %>%
  left_join(totals_lasso_coefs %>% select(name = term, int_coef = estimate), by = c('league_interaction' = 'name')) %>%
  mutate(int_coef = replace_na(int_coef, 0)) %>%
  group_by(target, side, .pred, int_coef) %>%
  summarise(man_pred = sum(value*estimate)+intercept) %>%
  mutate(man_pred = if_else(side == '2', man_pred + side_x + int_coef, man_pred+int_coef))

meta_model_preds %>%
  select(target, side, .pred, all_of(selected_models[-length(selected_models)])) %>%
  skimr::skim()

totals_lasso_models <- preds %>%
  distinct(wmkt, wxg, wgoals, xi) %>%
  mutate(across(c(wmkt:xi), ~round(., 5))) %>%
  mutate(model_id = glue::glue('model_{xi}_{wmkt}')) %>%
  filter(model_id %in% totals_lasso_coefs$term)
use_data(totals_lasso_models, overwrite = TRUE)




