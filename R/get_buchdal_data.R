#' get data from buchdal
#'
#' @param URL url code
#' @export
get_main_leagues <- function(URL, liigat){
  if(stringr::str_detect(URL, "xlsx")){
    temp = tempfile(fileext = ".xlsx")
  } else {
    temp = tempfile(fileext = ".xls")
  }
  download.file(URL, destfile=temp, mode='wb')

  Mainleagues <- temp %>%
    readxl::excel_sheets() %>%
    purrr::keep(., ~ . %in% liigat) %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel, path = temp) %>%
    purrr::keep(~purrr:::has_element(names(.),"PSCH"))

  Mainleagues <- purrr:::map(Mainleagues, dplyr::select, league = Div, date = Date,
                     home = HomeTeam, away = AwayTeam, PSCH, PSCD, PSCA, FTR, FTHG, FTAG) %>%
    dplyr::bind_rows() %>%
    na.omit() %>%
    dplyr::mutate(date = lubridate::as_date(date))

  Mainleagues <- implied::implied_probabilities(dplyr::select(Mainleagues, PSCH:PSCA), method = "wpo", normalize = TRUE)$probabilities %>%
    tibble::as_tibble() %>%
    dplyr::rename(FHP = PSCH, FDP = PSCD, FAP = PSCA) %>%
    dplyr::bind_cols(Mainleagues)

  message("Solving market goals...")

  Mainleagues <- goalmodel::expg_from_probabilities(dplyr::select(Mainleagues, FHP:FAP), rho = -0.13) %>%
    purrr::pluck(1) %>%
    tibble::as_tibble() %>%
    dplyr::select(mhxg = 1, maxg = 2) %>%
    dplyr::bind_cols(Mainleagues) %>%
    dplyr::select(league:PSCA, FHP:FAP, FTR:FTAG, mhxg, maxg) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringi::stri_trans_general(., id = "Latin-ASCII"))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~tolower(.))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringr::str_remove_all(., "[[:punct:]]")))

}
#' get extra leagues data from buchdal
#'
#' @param URL url code
#' @export
get_extra_leagues <- function(URL, liigat){
  if(stringr::str_detect(URL, "xlsx")){
    temp = tempfile(fileext = ".xlsx")
  } else {
    temp = tempfile(fileext = ".xls")
  }
  download.file(URL, destfile=temp, mode='wb')

  Mainleagues <- temp %>%
    readxl::excel_sheets() %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel, path = temp)

  Mainleagues <- Mainleagues %>%
    purrr::keep(~purrr::has_element(names(.),"PH")) %>%
    purrr::keep(names(.) %in% c("USA","MEX",'BRA','ARG')) %>%
    purrr::map(., . %>%
          dplyr::mutate(Season = substr(as.character(Season), 1, 4)) %>%
          dplyr::mutate(Date = lubridate::as_date(Date))
    )

  Mainleagues <- purrr:::map(Mainleagues, dplyr::select, league = League, date = Date, season = Season,
                     home = Home, away = Away, PSCH = PH, PSCD = PD, PSCA = PA, FTR = Res, FTHG = HG, FTAG = AG) %>%
    dplyr::bind_rows() %>%
    na.omit() %>%
    dplyr::filter(!stringr::str_detect(league, 'Copa')) %>%
    dplyr::group_by(league) %>%
    dplyr::filter(season == dplyr::last(season)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-season)

  Mainleagues <- implied::implied_probabilities(dplyr::select(Mainleagues, PSCH:PSCA), method = "wpo", normalize = TRUE)$probabilities %>%
    tibble::as_tibble() %>%
    dplyr::rename(FHP = PSCH, FDP = PSCD, FAP = PSCA) %>%
    dplyr::bind_cols(Mainleagues)

  message("Solving market goals...")

  Mainleagues <- goalmodel::expg_from_probabilities(dplyr::select(Mainleagues, FHP:FAP), rho = -0.13) %>%
    purrr::pluck(1) %>%
    tibble::as_tibble() %>%
    dplyr::select(mhxg = 1, maxg = 2) %>%
    dplyr::bind_cols(Mainleagues) %>%
    dplyr::select(league:PSCA, FHP:FAP, FTR:FTAG, mhxg, maxg) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringi::stri_trans_general(., id = "Latin-ASCII"))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~tolower(.))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringr::str_remove_all(., "[[:punct:]]")))

}
