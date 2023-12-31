pacman::p_load(bets, tidyverse)

seasons <- seq(2019, 2024, 1)
#check I2!
league_specs <-
  tibble(league.id = c(2436, 2386, 1928, 2196, 1980, 2432, 2036, 1842, 1977, 1843, 2242, 2663, 1834, 210697, 1817, 2037, 9999),
         league = c('I1', 'P1', 'N1', 'SP1', 'E0', 'SP2', 'F1', 'D1', 'E1', 'D2', 'Liga MX', 'MLS', 'Serie A', 'Liga Profesional', 'B1', 'F2', 'I2'),
         fbref_cntry = c('ITA','POR','NED','ESP','ENG','ESP','FRA','GER','ENG','GER','MEX','USA','BRA','ARG','BEL','FRA','ITA'),
         tier = c('1st','1st','1st','1st','1st','2nd','1st','1st','2nd','2nd','1st','1st','1st','1st','1st','2nd','2nd'))

fb_ref_grid <- expand_grid(seasons, league_specs) %>%
  #naille kolmelle vuosi yleensa -1
  filter(case_when(league %in% c('MLS', 'Serie A', 'Liga Profesional') ~ seasons < max(seasons),
                   TRUE ~ seasons == seasons))

fbref_data <- pmap_dfr(list(fb_ref_grid$fbref_cntry, fb_ref_grid$tier, fb_ref_grid$seasons), get_fbref_data) %>%
  arrange(date) %>%
  filter(!is.na(hg))

usethis::use_data(fbref_data, overwrite = TRUE)


urls <-
  tibble::tribble(
    ~season, ~filename,
    '2019', 'https://www.football-data.co.uk/mmz4281/1819/all-euro-data-2018-2019.xlsx',
    '2020', 'https://www.football-data.co.uk/mmz4281/1920/all-euro-data-2019-2020.xlsx',
    '2021', 'https://www.football-data.co.uk/mmz4281/2021/all-euro-data-2020-2021.xlsx',
    '2022', 'https://www.football-data.co.uk/mmz4281/2122/all-euro-data-2021-2022.xlsx',
    '2023', 'https://www.football-data.co.uk/mmz4281/2223/all-euro-data-2022-2023.xlsx',
    '2024', 'https://www.football-data.co.uk/mmz4281/2324/all-euro-data-2023-2024.xlsx'
  )

hist_buch_data <- get_historical_buchdata(urls)

usethis::use_data(hist_buch_data, overwrite = TRUE)

hist_totals_data <- get_historical_totals(urls)

usethis::use_data(hist_totals_data, overwrite = TRUE)
