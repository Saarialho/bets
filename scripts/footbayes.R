
# remove.packages(c("rstan", "StanHeaders"))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(footBayes)
library(bayesplot)
library(loo)
library(bets)
library(rsample)

data <- join_buch_fbref(bets::hist_buch_data, bets::fbref_data)
data

england <- data %>%
  filter(season == '1718', league == 'E0')

resamples <- england %>%
  arrange(date) %>%
  rsample::sliding_period(date,
                          'day',
                          lookback = Inf,
                          assess_stop = 1,
                          skip = 30,
                          step = 1) %>%
  filter(map_dbl(splits, ~ nrow(rsample::testing(.))) > 0)


train <- resamples$splits[[1]] %>%
  training()
test <- resamples$splits[[1]] %>%
  testing()

n_test <- nrow(test)

stan_data <- bind_rows(train, test) %>%
  select(season, home, away, FTHG, FTAG)


fit_stan <- stan_foot(data = stan_data,
                      model="double_pois",
                      prior = student_t(4,0, NULL), # 4 df
                      prior_sd = cauchy(0,25),
                      cores = 1,
                      iter = 200,
                      predict = n_test)


foot_prob(object = fit4_stan, data = italy_2000,
          home_team = "Reggina Calcio",
          away_team= "AC Milan")

library(engsoccerdata)
data(italy)
italy <- as.data.frame(italy)
italy_2000 <- subset(italy[, c(2,3,4,6,7)],
                     Season =="2000")
head(italy_2000)

### Fit Stan models
## no dynamics, no predictions
## 4 Markov chains, 'n_iter' iterations each

n_iter <- 200    # number of MCMC iterations


fit3_stan_t <- stan_foot(data = italy_2000,
                         model="double_pois",
                         prior = student_t(4,0, NULL), # 4 df
                         prior_sd = cauchy(0,25),
                         dynamic_type = "weekly",
                         cores = 1,
                         iter = n_iter,
                         predict = 10)  # double poisson


foot_round_robin(data = italy_2000, object = fit3_stan_t)


italy_2000 %>%
  as_tibble()
