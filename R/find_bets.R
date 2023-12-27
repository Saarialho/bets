#' helper fun to find bets
#' @param arviot data to ...
#' @param totals logical
#' @export
find_bets <- function(arviot, totals = TRUE){

  if(totals){

    hist_bets <- qs::qread("~/Documents/bets/output/totals_bets.rds")

    betit <- arviot %>%
      dplyr::filter(EV > 0.06, dts >= 2)

    betit <- betit %>%
      dplyr::filter(!(id %in% hist_bets$id)) %>%
      dplyr::mutate(bet = purrr::pmap_dbl(list(EV, kerroin, maxbet), bets::kelly_bet)) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))


  } else{

    hist_bets <- qs::qread("~/Documents/bets/output/multimodel_bets.rds")

    clv_model <- qs::qread(here::here('output', 'clv_reg.rds'))
    to_predict <- arviot %>%
      dplyr::mutate(moneyline = dplyr::case_when(kohde == '1' ~ mlh,
                                                 kohde == '2' ~ mld,
                                                 TRUE ~ mla)) %>%
      dplyr::mutate(moneyline = log(moneyline),
                    maxbet = log(maxbet),
                    plus_ev = factor(dplyr::if_else(EV > 0, 1, 0)))

    clv_pred <- predict(clv_model, to_predict)
    clv_pred <- tibble::tibble(clv_pred)

    arviot <- arviot %>%
      dplyr::bind_cols(clv_pred)

    betit <- arviot %>%
      dplyr::filter(clv_pred >= 0.015, dts >= 1) %>%
      dplyr::filter(!(id %in% hist_bets$id)) %>%
      dplyr::mutate(bet = purrr::pmap_dbl(list(clv_pred, kerroin, maxbet), bets::kelly_bet)) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))

  }

  return(betit)
}
