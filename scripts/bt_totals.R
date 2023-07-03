pacman::p_load(bets, tidyverse, implied, goalmodel)

data <- bets::join_buch_fbref(bets::hist_totals_data, bets::fbref_data)

data %>% skimr::skim()

data %>%
  select(season, league, h_xg) %>%
  dplyr::group_by(season, league) %>%
  skimr::skim()

data <- data %>%
  filter(case_when(league %in% c("E1", "N1", "P1") ~ season != "1718",
                   TRUE ~ season == season))

problems <- data %>%
  summarise(count_na = sum(is.na(h_xg)), .by = home) %>%
  arrange(desc(count_na)) %>%
  filter(count_na > 2)
problems

data
#add fair probs
data <- data %>%
  bind_cols(as_tibble(implied_probabilities(data %>% select(PSCH:PSCA), method = "wpo", normalize = TRUE)$probabilities) %>%
              rename(FHP = PSCH, FDP = PSCD, FAP = PSCA))

#add fair probs for totals
data <- data %>%
  bind_cols(as_tibble(implied_probabilities(data %>% select(o2.5:u2.5), method = "wpo", normalize = TRUE)$probabilities) %>%
                        rename(fo2.5 = o2.5, fu2.5 = u2.5))


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

rm(data)

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
                                skip = 30,
                                step = 1) %>%
        filter(map_dbl(splits, ~ nrow(rsample::testing(.))) > 0)

      predictions <- c()
      for (i in seq_len(nrow(resamples))){

        split <- purrr::pluck(resamples$splits, i)
        train <- split %>% rsample::training()
        test <- split %>% rsample::testing()

        fit_and_pred <- suppressWarnings(
          weights %>%
            mutate(data = list(train)) %>%
            unnest(c(data)) %>%
            mutate(hwxg = wmkt*mhxg + wxg*h_xg + wgoals*FTHG,
                   awxg = wmkt*maxg + wxg*a_xg + wgoals*FTAG) %>%
            group_nest(wmkt, wxg, wgoals, xi) %>%
            mutate(fit = map2(data, xi, bets::fit_multimixture_model)) %>%
            select(-data) %>%
            mutate(test = list(test)) %>%
            mutate(pred = map2(fit, test,
                               purrr::possibly(~goalmodel::predict_ou(.x, .y$home, .y$away, return_df = TRUE) %>% as_tibble(),
                                                   otherwise = NULL,
                                                   quiet = TRUE)))
        )

        preds <- fit_and_pred %>%
          select(-fit) %>%
          unnest(c(test, pred)) %>%
          group_nest(wmkt, wxg, wgoals, xi)

        predictions <- predictions %>% bind_rows(preds)
      }

      qs::qsave(predictions, here::here('output', glue::glue('total_{league}_{season}.rds')))

    },
    error=function(error_message) {message(error_message)}
  )
}
# Stop the cluster
stopCluster(cl)
