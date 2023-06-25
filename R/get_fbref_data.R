#' get fbref data
#'
#' @param seasons a vector of yearst
#' @return a tibble containing data.
#' @export
get_fbref_data <- function(seasons){

  longer_leagues <- worldfootballR::fb_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA", 'USA'),
                                                 gender = "M",
                                                 season_end_year = seasons,
                                                 tier = c("1st"))

  short_seasons <- seasons[seasons > 2018]

  shorter_leagues <- worldfootballR::fb_match_results(country = c('NED', 'MEX', 'POR', 'BRA'),
                                                 gender = "M",
                                                 season_end_year = short_seasons,
                                                 tier = c("1st"))


  second_tier <- worldfootballR::fb_match_results(country = c("ENG"), gender = "M",
                                                  season_end_year = short_seasons,
                                                  tier = c("2nd"))
  longer_leagues %>%
    dplyr::bind_rows(shorter_leagues) %>%
    dplyr::bind_rows(second_tier) %>%
    tibble::as_tibble() %>%
    dplyr::select(league = Competition_Name, season = Season_End_Year, date = Date,
                  home = Home, away = Away, hg = HomeGoals, ag = AwayGoals, h_xg = Home_xG, a_xg = Away_xG) %>%
    mutate(across(c(home, away), ~stringi::stri_trans_general(., id = "Latin-ASCII"))) %>%
    mutate(across(c(home, away), ~tolower(.))) %>%
    dplyr::mutate(across(c(home, away), ~str_remove_all(., "[[:punct:]]")))
}


