
Read_excel_files <- function(URL){
  if(str_detect(URL, "xlsx")){
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
    purrr::keep(~purrr::has_element(names(.),"PSCH")) %>%
    purrr::keep(~purrr::has_element(names(.),"PSH"))

  Mainleagues <- purrr::map(Mainleagues, dplyr::select, league = Div, date = Date, home = HomeTeam, away = AwayTeam,
                            PSCH, PSCD, PSCA, FTR, FTHG, FTAG) %>%
    dplyr::bind_rows() %>%
    na.omit()

}
Read_new_leagues <- function(URL){
  if(str_detect(URL, "xlsx")){
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
    purrr::keep(names(.) %in% c("USA","MEX",'BRA', 'ARG')) %>%
    purrr::map(~dplyr::mutate(., Season = as.character(Season))) %>%
    purrr::map(~dplyr::mutate(., Season = substr(Season, 1, 4)))

  Mainleagues <- purrr::map(Mainleagues, dplyr::select, league = League, date = Date,
                            season = Season, home = Home, away = Away, PSCH = PH, PSCD = PD,
                            PSCA = PA, FTR = Res, FTHG = HG, FTAG = AG) %>%
    dplyr::bind_rows() %>%
    na.omit()

}

#' get historical data of buchal for all years
#'
#' @param urls for mainleagues
#' @export
get_historical_buchdata <- function(urls){

  new_leagues <- Read_new_leagues('https://www.football-data.co.uk/new/new_leagues_data.xlsx') %>%
    dplyr::filter(stringr::str_detect(league, 'MLS|Liga MX|Serie|Liga Profe')) %>%
    dplyr::filter(!stringr::str_detect(league, 'Copa')) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::filter(season >= 2018) %>%
    dplyr::mutate(season = dplyr::case_when(season == '2018' ~ '1819',
                              season == '2019' ~ '1920',
                              season == '2020' ~ '2021',
                              season == '2021' ~ '2122',
                              season == '2022' ~ '2223',
                              season == '2023' ~ '2324',
                              season == '2024' ~ '2425',
                              season == '2025' ~ '2526'))

  All_leagues <- urls %>%
    dplyr::mutate(data = purrr::map(filename, Read_excel_files)) %>%
    dplyr::select(-filename)

  liigat <- c("E0", "D1", "SP1", "I1", "F1", "E1", "P1", "N1", "D2", "SP2", "I2")

  All_leagues <- All_leagues %>%
    tidyr::unnest(data) %>%
    dplyr::filter(league %in% liigat)

  dplyr::bind_rows(All_leagues, new_leagues) %>%
    dplyr::filter(dplyr::case_when(
      league %in% c("E1", "N1", "P1", 'Serie A') ~ season != "1819",
      TRUE ~ season == season)) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringi::stri_trans_general(., id = "Latin-ASCII"))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~tolower(.))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringr::str_remove_all(., "[[:punct:]]")))
}

Read_totals <- function(URL){
  if(str_detect(URL, "xlsx")){
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
    purrr::keep(~purrr::has_element(names(.),"PC<2.5"))

  Mainleagues <- purrr::map(Mainleagues, dplyr::select, league = Div, date = Date, home = HomeTeam, away = AwayTeam,
                            PSCH, PSCD, PSCA, FTR, FTHG, FTAG, o2.5 = `PC>2.5`, u2.5 = `PC<2.5`) %>%
    dplyr::bind_rows() %>%
    na.omit()

}

get_historical_totals <- function(urls){

  All_leagues <- urls %>%
    dplyr::mutate(data = purrr::map(filename, Read_totals)) %>%
    dplyr::select(-filename)

  #tahan voisi lisata D2, SP2 ja I2?
  liigat <- c("E0", "D1", "SP1", "I1", "F1", "E1", "P1", "N1")

  All_leagues <- All_leagues %>%
    tidyr::unnest(data) %>%
    dplyr::filter(league %in% liigat)

  All_leagues %>%
    dplyr::filter(dplyr::case_when(
      league %in% c("E1", "N1", "P1", 'Serie A') ~ season != "1819",
      TRUE ~ season == season)) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringi::stri_trans_general(., id = "Latin-ASCII"))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~tolower(.))) %>%
    dplyr::mutate(dplyr::across(c(home, away), ~stringr::str_remove_all(., "[[:punct:]]")))


}
