#' get odds from pinnacle
#'
#' @param liigat vector of league codes
#' @export
get_pinnacle_odds <- function(liigat){
  soccer_id <- 29
  Fixtures <- pinnacle.API::GetFixtures(soccer_id, liigat, islive = FALSE) %>%
    dplyr::select(events.id=league.events.id, events.home=league.events.home, events.away=league.events.away)

  Odds <- pinnacle.API::GetOdds(soccer_id, liigat, islive = FALSE, oddsformat = "DECIMAL", tableformat = "subtables") %>%
    dplyr::filter(leagues.events.periods.number == 0) %>%
    dplyr::select(leagues.id,
           events.id = leagues.events.id,
           date = leagues.events.periods.cutoff,
           moneyline.home = leagues.events.periods.moneyline.home,
           moneyline.draw = leagues.events.periods.moneyline.draw,
           moneyline.away = leagues.events.periods.moneyline.away,
           periods.spreads = leagues.events.periods.spreads,
           maxbet = leagues.events.periods.maxMoneyline) %>%
    dplyr::mutate(date = as_date(lubridate::ymd_hms(date)))

  soccer_data <- Fixtures %>%
    dplyr::left_join(Odds, by = c("events.id")) %>%
    na.omit() %>%
    dplyr::select(leagues.id, date, events.home, events.away, moneyline.home, moneyline.draw, moneyline.away, periods.spreads, maxbet)

  return(soccer_data)

  if(nrow(soccer_data) == 0){
    cat("OBS! Ei tulevia peleja", Loppu, "paivan sisalla")
  }
}
