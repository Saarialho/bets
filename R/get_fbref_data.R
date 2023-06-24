#' get fbref data
#'
#' @param seasons a vector of yearst
#' @return a tibble containing data.
#' @export
get_fbref_data <- function(seasons){
  first_tier <- worldfootballR::fb_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA", 'USA', 'NED', 'MEX', 'POR'),
                                                 gender = "M",
                                                 season_end_year = seasons,
                                                 tier = c("1st"))

  second_tier <- worldfootballR::fb_match_results(country = c("ENG"), gender = "M",
                                                  season_end_year = seasons,
                                                  tier = c("2nd"))
  first_tier %>%
    dplyr::bind_rows(second_tier) %>%
    tibble::as_tibble() %>%
    dplyr::select(league = Competition_Name, season = Season_End_Year, date = Date,
                  home = Home, away = Away, hg = HomeGoals, ag = AwayGoals, h_xg = Home_xG, a_xg = Away_xG)
}


