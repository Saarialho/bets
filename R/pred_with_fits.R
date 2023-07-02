#' predict with  fits
#' @param fits goalmodel object
#' @param totals logical
#' @param pin_odds dataset
#' @export
pred_with_fits <- function(fits, totals = TRUE, pin_odds){

  if(totals){
    suppressWarnings(
      preds <- fits %>%
        dplyr::left_join(pinnacle_odds) %>%
        dplyr::mutate(pelit = purrr::map2(fit, pelit, purrr::possibly(predict_totals, otherwise = NA))) %>%
        dplyr::select(league, model_id, pelit, teams) %>%
        tidyr::unnest(pelit, .drop = FALSE)
    )
  } else {
    suppressWarnings(
      preds <- fits %>%
        dplyr::left_join(pinnacle_odds) %>%
        dplyr::mutate(pelit = purrr::map2(fit, pelit, purrr::possibly(predict_1x2, otherwise = NA))) %>%
        dplyr::select(league, model_id, pelit, teams) %>%
        tidyr::unnest(pelit, .drop = FALSE)
    )
  }
  return(preds)
}
