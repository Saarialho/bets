#' aggregate predictions
#'
#' @param preds predictions
#' @param totals logical
#' @export
aggregate_predictions <- function(preds, totals = FALSE){

  if(totals){

    lasso_coefs <- qs::qread(here::here('models', 'totals_lasso_coefs.rds'))

    intercept <- dplyr::pull(dplyr::filter(lasso_coefs, term == '(Intercept)'), estimate)

    totals <- preds %>%
      dplyr::select(date, team1, team2, periods.totals) %>%
      dplyr::distinct(date, team1, team2, .keep_all = TRUE)

    arviot <- preds %>%
      dplyr::select(-teams, -mlh, -mld, -mla, -periods.totals) %>%
      tidyr::pivot_longer(cols = prob_under:prob_over, names_to = 'pred_side', values_to = 'pred') %>%
      dplyr::mutate(side = dplyr::if_else(pred_side == 'prob_over', '1', '2')) %>%
      dplyr::mutate(league_interaction = glue::glue('side{side}_x_league{league}')) %>%
      dplyr::mutate(league_dummy = glue::glue('league_{league}')) %>%
      dplyr::mutate(league_dummy = dplyr::if_else(league == 'Serie A', 'league_Serie.A', league_dummy)) %>%
      dplyr::mutate(side_dummy = glue::glue('side_X{side}')) %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id = term, estimate)) %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id = term, int_coef = estimate), by = c('league_interaction' = 'model_id')) %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id = term, dummy_coef = estimate), by = c('league_dummy' = 'model_id')) %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id = term, side_coef = estimate), by = c('side_dummy' = 'model_id')) %>%
      dplyr::mutate(dplyr::across(contains('_coef'), ~replace_na(., 0))) %>%
      dplyr::select(-side) %>%
      dplyr::rename(side = pred_side) %>%
      dplyr::summarise(pred = sum(pred*estimate)+intercept,
                .by = c(date, league, team1, team2, maxbet, int_coef, dummy_coef, side_coef, side)) %>%
      dplyr::mutate(pred = pred + int_coef + dummy_coef + side_coef) %>%
      dplyr::select(-dplyr::ends_with('_coef')) %>%
      tidyr::pivot_wider(names_from = side, values_from = pred) %>%
      dplyr::left_join(totals) %>%
      tidyr::unnest(periods.totals) %>%
      dplyr::group_by(date, league, team1) %>%
      tidyr::fill(c(max, altLineId), .direction = 'downup') %>%
      dplyr::ungroup() %>%
      dplyr::filter(points == 2.5) %>%
      dplyr::mutate(EV_over = prob_over*over-1,
             EV_under = prob_under*under-1) %>%
      dplyr::mutate(EV = pmax(EV_over, EV_under), .by = c(team1, date)) %>%
      dplyr::mutate(kerroin = dplyr::case_when(EV == EV_over ~ over,
                                 EV == EV_under ~ under),
             kohde = factor(dplyr::case_when(EV == EV_over ~ 1,
                                      TRUE ~ 2)),
             dts = as.numeric(date-Sys.Date()),
             id = paste(date, team1, team2, sep = "-"), .by = c(team1, date)) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3))) %>%
      dplyr::select(-dplyr::any_of('max'))

  } else {

    lasso_coefs <- qs::qread(here::here('models', 'lasso_coefs.rds'))

    intercept <- dplyr::pull(dplyr::filter(lasso_coefs, term == '(Intercept)'), estimate)

    spreads <- preds %>%
      dplyr::select(date, team1, team2, periods.spreads) %>%
      dplyr::distinct(date, team1, team2, .keep_all = TRUE)

    arviot <- preds %>%
      dplyr::select(-teams) %>%
      tidyr::pivot_longer(cols = p1:p2, names_to = 'pred_side', values_to = 'pred') %>%
      dplyr::mutate(side = dplyr::case_when(pred_side == 'p1' ~ '1',
                                     pred_side == 'pd' ~ 'X',
                                     TRUE ~ '2')) %>%
      dplyr::mutate(league_interaction = glue::glue('side{side}_x_league{league}')) %>%
      dplyr::mutate(league_dummy = glue::glue('league_{league}')) %>%
      dplyr::mutate(league_dummy = dplyr::if_else(league == 'Serie A', 'league_Serie.A', league_dummy)) %>%
      #tahan ei tuu X
      dplyr::mutate(side_dummy = glue::glue('side_{side}')) %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id = term, estimate)) %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id = term, int_coef = estimate), by = c('league_interaction' = 'model_id')) %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id = term, dummy_coef = estimate), by = c('league_dummy' = 'model_id')) %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id = term, side_coef = estimate), by = c('side_dummy' = 'model_id')) %>%
      dplyr::mutate(dplyr::across(contains('_coef'), ~replace_na(., 0))) %>%
      dplyr::select(-side) %>%
      dplyr::rename(side = pred_side) %>%
      dplyr::summarise(pred = sum(pred*estimate)+intercept,
                .by = c(date, league, team1, team2, mlh, mld, mla, maxbet, side, int_coef, dummy_coef, side_coef)) %>%
      dplyr::mutate(pred = pred + int_coef + dummy_coef + side_coef) %>%
      dplyr::select(-dplyr::ends_with('_coef')) %>%
      tidyr::pivot_wider(names_from = side, values_from = pred) %>%
      dplyr::left_join(spreads) %>%
      tidyr::unnest(periods.spreads) %>%
      dplyr::group_by(date, league, team1) %>%
      tidyr::fill(c(max,altLineId), .direction = 'downup') %>%
      dplyr::ungroup() %>%
      dplyr::filter(hdp %in% c(0,0.5,-0.5)) %>%
      dplyr::mutate(EV1 = p1*mlh-1,
             EVD = pd*mld-1,
             EV2 = p2*mla-1) %>%
      dplyr::mutate(EV1_hdp = dplyr::case_when(hdp == 0 ~ p1/(1-pd)*home-1,
                                 hdp == -0.5 ~ p1*home-1,
                                 hdp == 0.5 ~ (p1+pd)*home-1),
             EV2_hdp = dplyr::case_when(hdp == 0 ~ p2/(1-pd)*away-1,
                                 hdp == -0.5 ~ (p2+pd)*away-1,
                                 hdp == 0.5 ~ p2*away-1)) %>%
      dplyr::mutate(max_hdp = pmax(EV1_hdp, EV2_hdp), .by = c(team1, date)) %>%
      dplyr::filter((max_hdp == max(max_hdp)) %>% tidyr::replace_na(TRUE), .by = c(team1, date)) %>%
      dplyr::mutate(EV = pmax(EV1,EVD,EV2,EV1_hdp,EV2_hdp, na.rm = TRUE),
             kerroin = dplyr::case_when(EV == EV1 ~ mlh,
                                 EV == EVD ~ mld,
                                 EV == EV2 ~ mla,
                                 EV == EV1_hdp ~ home,
                                 EV == EV2_hdp ~ away),
             kohde = factor(dplyr::case_when(EV == EV1 | EV == EV1_hdp ~ 1,
                                      EV == EVD ~ 2,
                                      TRUE ~ 3)),
             dts = as.numeric(date-Sys.Date()),
             id = paste(date, team1, team2, sep = "-"), .by = c(team1, date)) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3))) %>%
      dplyr::select(-dplyr::any_of('max'))
  }

  return(arviot)
}
