#' fit multimixture model
#'
#' @param games games dataset
#' @export
fit_multimixture_model <- function(games) {

  goalmodel:::goalmodel(goals1 = games$HWxG, goals2 = games$AWxG,
            team1 = games$Home, team2=games$Away,
            weights = goalmodel::weights_dc(games$date, xi=games$xi[1]),
            model = "gaussian")
}
