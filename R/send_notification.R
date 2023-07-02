#' helper fun to send bets
#' @param betit betit
#' @export
send_notification <- function(betit){

  if(nrow(betit) > 0){

    notification <- betit %>%
      dplyr::arrange(desc(league)) %>%
      dplyr::select(team1, team2, mlh:mla, kerroin, bet)

    pushoverr::set_pushover_user(user = Sys.getenv('PUSHOVER_USER'))
    pushoverr::set_pushover_app(token = Sys.getenv('PUSHOVER_APP'))

    png("bets.png", height=400, width=1400, res = 200)
    p <- gridExtra::grid.arrange(gridExtra::tableGrob(notification))
    dev.off()

    pushoverr::pushover(
      message = "Look at those bets!",
      attachment = "bets.png"
    )

  }

}
