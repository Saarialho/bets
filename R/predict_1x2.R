#' fit multimixture model
#'
#' @param model goalmodel object
#' @param data dataset
#' @export
predict_1x2 <- function(model, data){
  goalmodel::predict_result(model, data$events.home, data$events.away, return_df = TRUE) %>%
    dplyr::bind_cols(data) %>%
    dplyr::select(date, team1,team2, p1, pd, p2,
           mlh = moneyline.home, mld = moneyline.draw, mla = moneyline.away,
           periods.spreads, maxbet)
}
