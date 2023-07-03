#' aggregate predictions
#'
#' @param preds predictions
#' @param totals logical
#' @export
aggregate_predictions <- function(preds, totals = FALSE){

  if(totals){

    coefs <- qs::qread(here::here('output', 'totals_lasso_coefs.rds'))

    intercept <- dplyr::pull(dplyr::filter(coefs, term == '(Intercept)'), estimate)
    side_x <- dplyr::pull(dplyr::filter(coefs, term == 'side_X2'), estimate)
    league_F1 <- dplyr::pull(dplyr::filter(coefs, term == 'league_F1'), estimate)

    totals <- preds %>%
      dplyr::select(date, team1, team2, periods.totals) %>%
      dplyr::distinct(date, team1, team2, .keep_all = TRUE)

    arviot <- preds %>%
      dplyr::select(-teams, -mlh, -mld, -mla, -periods.totals) %>%
      tidyr::pivot_longer(cols = prob_under:prob_over, names_to = 'pred_side', values_to = 'pred') %>%
      dplyr::mutate(side = dplyr::if_else(pred_side == 'prob_over', '1', '2')) %>%
      dplyr::mutate(league_interaction = glue::glue('side{side}_x_league{league}')) %>%
      dplyr::left_join(coefs %>% dplyr::select(model_id  = term, estimate), by = 'model_id') %>%
      dplyr::left_join(coefs %>% dplyr::select(name = term, int_coef = estimate), by = c('league_interaction' = 'name')) %>%
      dplyr::select(-side) %>%
      dplyr::rename(side = pred_side) %>%
      dplyr::mutate(int_coef = tidyr::replace_na(int_coef, 0)) %>%
      dplyr::summarise(pred = sum(pred*estimate)+intercept,
                .by = c(date, league, team1, team2, maxbet, int_coef, side)) %>%
      #interaktio termi
      dplyr::mutate(pred = pred + int_coef) %>%
      dplyr::select(-int_coef) %>%
      #side korjaus
      dplyr::mutate(pred = dplyr::if_else(side == 'prob_under', pred + side_x, pred)) %>%
      #liiga korjaus
      dplyr::mutate(pred = dplyr::if_else(league == 'F1', pred + league_F1, pred)) %>%

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
    intercept <- dplyr::pull(dplyr::filter(bets::lasso_coefs, term == '(Intercept)'), estimate)
    side_x <- dplyr::pull(dplyr::filter(bets::lasso_coefs, term == 'side_X'), estimate)

    spreads <- preds %>%
      dplyr::select(date, team1, team2, periods.spreads) %>%
      dplyr::distinct(date, team1, team2, .keep_all = TRUE)

    arviot <- preds %>%
      dplyr::select(-teams) %>%
      tidyr::pivot_longer(cols = p1:p2, names_to = 'side', values_to = 'pred') %>%
      dplyr::left_join(lasso_coefs %>% dplyr::select(model_id  = term, estimate), by = 'model_id') %>%
      summarise(pred = sum(pred*estimate)+intercept,
                .by = c(date, league, team1, team2, mlh, mld, mla, maxbet, side)) %>%
      dplyr::mutate(pred = if_else(side == 'pd', pred + side_x, pred)) %>%
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
