
seasons <- seq(2018, 2023, 1)
fbref_data <- get_fbref_data(seasons)

urls <- list(
  "https://www.football-data.co.uk/mmz4281/1718/all-euro-data-2017-2018.xlsx",
  "https://www.football-data.co.uk/mmz4281/1819/all-euro-data-2018-2019.xlsx",
  "https://www.football-data.co.uk/mmz4281/1920/all-euro-data-2019-2020.xlsx",
  "https://www.football-data.co.uk/mmz4281/2021/all-euro-data-2020-2021.xlsx",
  "https://www.football-data.co.uk/mmz4281/2122/all-euro-data-2021-2022.xlsx",
  "https://www.football-data.co.uk/mmz4281/2223/all-euro-data-2022-2023.xlsx"
)

hist_buch_data <- get_historical_buchdata(urls)

use_data(hist_buch_data, overwrite = TRUE)
use_data(fbref_data, overwrite = TRUE)
