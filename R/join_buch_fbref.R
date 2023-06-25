
#' @title Join Buch Data and Fbref Data
#'
#' @description This function joins data using a data.table join.
#'
#' @param buch_data data
#' @param fbref_data data
#' @return A tibble containing the joined data.
#' @export
join_buch_fbref <- function(buch_data, fbref_data){

  fbref_data <- fbref_data %>%
    bets::replace_team_names(home, away, bets::team_dictionary()$fbref_name, bets::team_dictionary()$buch_name) %>%
    select(date:a_xg) %>%
    select(-hg, -ag)

  buch_data <- buch_data %>%
    mutate(date_start = date - 5,
           date_end = date + 5)

  left_join(buch_data, fbref_data, by = join_by(home == home, away == away, date_start <= date, date_end >= date)) %>%
    select(-date.y, date_start, date_end) %>%
    rename(date = date.x)
}

