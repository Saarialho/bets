#' get fbref data
#'
#' @param seasons a vector of years
#' @param countries a vector of countries
#' @param tiers a vector of tiers
#' @return a tibble containing data.
#' @export
get_fbref_data <- function(countries, tiers, seasons){

  data <- worldfootballR::fb_match_results(country = countries,
                                                 gender = "M",
                                                 season_end_year = seasons,
                                                 tier = tiers)

  rename_lookup <-
    c(league = 'Competition_Name', season = 'Season_End_Year', date = 'Date', home = 'Home',
      away = 'Away', hg = 'HomeGoals', ag = 'AwayGoals', h_xg = 'Home_xG', a_xg = 'Away_xG')

  select_lookup <- c('league', 'season', 'date', 'home', 'away', 'hg', 'ag', 'h_xg', 'a_xg')

  data %>%
    tibble::as_tibble() %>%
    dplyr::rename(any_of(rename_lookup)) %>%
    dplyr::select(any_of(select_lookup)) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringi::stri_trans_general(., id = "Latin-ASCII"))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~tolower(.))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringr::str_remove_all(., "[[:punct:]]"))) %>%
    #santos seka MX etta Bras liigoissa
    dplyr::mutate(dplyr::across(c(home, away), ~ dplyr::if_else(league == 'Liga MX' & . == 'santos', 'santos laguna', .))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~ dplyr::if_else(league == 'Argentine Primera División' & . == 'arsenal', 'arsenal sarandi', .)))
}


