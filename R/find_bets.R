#' helper fun to find bets
#' @param arviot data to ...
#' @param totals logical
#' @export
find_bets <- function(arviot, totals = TRUE){

  if(totals){

    hist_bets <- qs::qread("~/Documents/bets/output/totals_bets.rds")

    betit <- arviot %>%
      dplyr::filter(EV > 0.06, dts >= 2)

  } else{

    hist_bets <- qs::qread("~/Documents/bets/output/multimodel_bets.rds")

    betit <- arviot %>%
      dplyr::filter(EV > 0.06, dts >= 2)

  }

  betit <- betit %>%
    dplyr::filter(!(id %in% hist_bets$id)) %>%
    dplyr::mutate(bet = purrr::pmap_dbl(list(EV, kerroin, maxbet), bets::kelly_bet)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))

  return(betit)
}
