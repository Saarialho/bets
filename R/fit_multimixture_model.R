#' fit multimixture model
#'
#' @param games games dataset
#' @param xi time decay
#' @export
fit_multimixture_model <- function(games, xi) {

  goalmodel:::goalmodel(goals1 = games$hwxg, goals2 = games$awxg,
            team1 = games$home, team2=games$away,
            weights = goalmodel::weights_dc(games$date, xi=xi),
            model = "gaussian")
}
