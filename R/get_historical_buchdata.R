
Read_excel_files <- function(URL){
  if(str_detect(URL, "xlsx")){
    temp = tempfile(fileext = ".xlsx")
  } else {
    temp = tempfile(fileext = ".xls")
  }
  download.file(URL, destfile=temp, mode='wb')

  Mainleagues <- temp %>%
    excel_sheets() %>%
    set_names() %>%
    map(read_excel, path = temp)

  Mainleagues <- Mainleagues %>%
    keep(~has_element(names(.),"PSCH")) %>%
    keep(~has_element(names(.),"PSH"))

  Mainleagues <- map(Mainleagues, select, league = Div, date = Date, home = HomeTeam, away = AwayTeam, PSCH, PSCD, PSCA, FTR, FTHG, FTAG) %>%
    bind_rows() %>%
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
    excel_sheets() %>%
    set_names() %>%
    map(read_excel, path = temp)

  Mainleagues <- Mainleagues %>%
    keep(~has_element(names(.),"PH")) %>%
    keep(names(.) %in% c("USA","MEX",'BRA')) %>%
    map(~mutate(., Season = as.character(Season))) %>%
    map(~mutate(., Season = substr(Season, 1, 4)))

  Mainleagues <- map(Mainleagues, select, league = League, date = Date, season = Season, home = Home, away = Away, PSCH = PH, PSCD = PD, PSCA = PA, FTR = Res, FTHG = HG, FTAG = AG) %>%
    bind_rows() %>%
    na.omit()

}

#' get historical data of buchal for all years
#'
#' @param urls for mainleagues
#' @export
get_historical_buchdata <- function(urls){
  new_leagues <- Read_new_leagues('https://www.football-data.co.uk/new/new_leagues_data.xlsx') %>%
    filter(str_detect(league, 'MLS|Liga MX|Serie')) %>%
    mutate(date = as_date(date)) %>%
    filter(season >= 2018) %>%
    mutate(season = case_when(season == '2018' ~ '1819',
                              season == '2019' ~ '1920',
                              season == '2020' ~ '2021',
                              season == '2021' ~ '2122',
                              season == '2022' ~ '2223',
                              season == '2023' ~ '2324',
                              season == '2024' ~ '2425',
                              season == '2025' ~ '2526'))

  All_leagues <- tibble(season = c("1718","1819","1920", "2021", "2122", "2223"),
                        filename = urls)

  All_leagues <- All_leagues %>%
    mutate(data = map(filename, Read_excel_files)) %>%
    select(-filename)

  liigat <- c("E0", "D1", "SP1", "I1", "F1", "E1", "P1", "N1")

  All_leagues <- All_leagues %>%
    unnest(data) %>%
    filter(league %in% liigat)

  bind_rows(All_leagues, new_leagues) %>%
    filter(case_when(
      league %in% c("E1", "N1", "P1", 'Serie A') ~ season != "1819",
      TRUE ~ season == season)) %>%
    mutate(date = as_date(date)) %>%
    mutate(across(c(home, away), ~stringi::stri_trans_general(., id = "Latin-ASCII"))) %>%
    mutate(across(c(home, away), ~tolower(.))) %>%
    mutate(across(c(home, away), ~str_remove_all(., "[[:punct:]]")))
}


