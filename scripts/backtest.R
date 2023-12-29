library(bets)
library(tidyverse)
library(implied)
library(goalmodel)

data <- join_buch_fbref(bets::hist_buch_data, bets::fbref_data)

data %>%
  select(season, league, h_xg) %>%
  group_by(season, league) %>%
  skimr::skim()

bets::hist_buch_data %>% count(league)
bets::fbref_data %>% count(league)

buch_names <- bets::hist_buch_data %>%
  filter(league == 'MLS') %>%
  count(home) %>%
  select(buch = home, nbu = n)

fbref_names <- bets::fbref_data %>%
  filter(league == 'Major League Soccer') %>%
  count(home) %>%
  select(fbref = home, nref = n)

fbref_names %>%
  filter(!(fbref %in% buch_names$buch))

buch_names %>%
  bind_cols(fbref_names)

data %>% skimr::skim()

data <- data %>%
  filter(case_when(league %in% c("E1", "N1", "P1") ~ season != "1718",
                   TRUE ~ season == season)) %>%
  filter(case_when(league %in% c("MLS") ~ season != "1819",
                   TRUE ~ season == season)) %>%
  filter(league != 'I2')

problems <- data %>%
  group_by(home) %>%
  summarise(count_na = sum(is.na(h_xg))) %>%
  arrange(desc(count_na)) %>%
  filter(count_na > 2)
problems

#add fair probs
data <- data %>%
  bind_cols(as_tibble(implied_probabilities(data %>% select(PSCH:PSCA), method = "wpo", normalize = TRUE)$probabilities) %>%
              rename(FHP = PSCH, FDP = PSCD, FAP = PSCA))

# add market xgs
data <- data %>%
  bind_cols(data %>%
  select(FHP:FAP) %>%
  expg_from_probabilities(., rho = -0.13) %>%
  .$expg %>%
  as_tibble(.name_repair = ~paste0("xg", seq_along(.))) %>%
  select(mhxg = xg1, maxg = xg2))

leagues <- data %>%
  select(-contains('date_')) %>%
  mutate(h_xg = if_else(is.na(h_xg), FTHG, h_xg),
        a_xg = if_else(is.na(a_xg), FTAG, a_xg)) %>%
  group_nest(league, season)

leagues %>%
  unnest(data) %>%
  skimr::skim()

rm(data, fbref_names, problems, buch_names)

weights <- t(replicate(1000, diff(c(0, sort(runif(2)), 1))) ) %>%
  as_tibble() %>%
  select(wmkt = V1, wxg = V2, wgoals = V3) %>%
  rowwise() %>%
  mutate(xi = runif(1, 0.00, 0.1)) %>%
  ungroup()
weights


library(foreach)
library(doParallel)
# Register parallel backend
cl <- makeCluster(8)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(tidyverse)
  library(goalmodel)
  library(bets)
  library(rsample)
})

# Replace for loop with foreach loop
foreach(row = seq_len(nrow(leagues))) %dopar% {
  tryCatch(
    {

      season_data <- leagues %>% slice(row)

      season <- season_data$league
      league <- season_data$season

      message(glue::glue('Current league & season: {league} & {season}'))

      resamples <- season_data %>%
        unnest(c(data)) %>%
        arrange(date) %>%
        rsample::sliding_period(date,
                                'day',
                                lookback = Inf,
                                assess_stop = 1,
                                skip = 35,
                                step = 1) %>%
        filter(map_dbl(splits, ~ nrow(rsample::testing(.))) > 0)

      predictions <- c()
      for (i in seq_len(nrow(resamples))){

        split <- pluck(resamples$splits, i)
        train <- split %>% rsample::training()
        test <- split %>% rsample::testing()

        fit_and_pred <- suppressWarnings(
          weights %>%
            mutate(data = list(train)) %>%
            unnest(c(data)) %>%
            mutate(hwxg = wmkt*mhxg + wxg*h_xg + wgoals*FTHG,
                   awxg = wmkt*maxg + wxg*a_xg + wgoals*FTAG) %>%
            group_nest(wmkt, wxg, wgoals, xi) %>%
            mutate(fit = map2(data, xi, fit_multimixture_model)) %>%
            select(-data) %>%
            mutate(test = list(test)) %>%
            mutate(pred = map2(fit, test, possibly(~goalmodel::predict_result(.x, .y$home, .y$away, return_df = TRUE) %>% as_tibble(),
                                                   otherwise = NULL,
                                                   quiet = TRUE)))
        )

        preds <- fit_and_pred %>%
          select(-fit) %>%
          unnest(c(test, pred)) %>%
          mutate(obs = if_else(FTHG > FTAG, 1, if_else(FTHG == FTAG, 2, 3))) %>%
          group_nest(wmkt, wxg, wgoals, xi)

        predictions <- predictions %>% bind_rows(preds)
      }

      qs::qsave(predictions, here::here('output', glue::glue('{league}_{season}.rds')))

    },
    error=function(error_message) {message(error_message)}
  )
}

# Stop the cluster
stopCluster(cl)

