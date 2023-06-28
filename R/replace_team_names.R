
#' replaces team names in data frame
#'
#' @param data account name
#' @param team1 account name
#' @param team2 account name
#' @param pattern account name
#' @param replacement account name
#' @examples
#' hist_buch_data %>% replace_team_names(home, away, team_dictionary()$buch_name, team_dictionary()$fbref_name)
#' @export
replace_team_names <- function(data, team1, team2, pattern, replacement){

  # Add word boundaries to the pattern
  pattern <- paste0("(?<! )\\b", pattern, "\\b(?! )")

  names <-
    dplyr::tibble(pattern = pattern,
           replacement = replacement) %>%
    dplyr::filter(pattern != replacement) %>%
    dplyr::arrange(desc(nchar(pattern))) # Sort by length of pattern

  # Making the named replacement vector
  replacements <- c(names$replacement)
  names(replacements) <- c(names$pattern)

  data %>%
    dplyr::group_by(league) %>%
    dplyr::mutate(dplyr::across(c({{team1}},{{team2}}), ~stringr::str_replace_all(., pattern = replacements))) %>%
    dplyr::ungroup()
}

