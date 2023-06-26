

data <- join_buch_fbref(bets::hist_buch_data, bets::fbref_data)

data %>% skimr::skim()

data %>%
  select(season, league, h_xg) %>%
  group_by(season, league) %>%
  skimr::skim()

data <- data %>%
  filter(case_when(league %in% c("E1", "N1", "P1") ~ season != "1718",
                   TRUE ~ season == season))

problems <- data %>%
  group_by(home) %>%
  summarise(count_na = sum(is.na(h_xg))) %>%
  arrange(desc(count_na)) %>%
  filter(count_na > 2)
problems

data

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
  select(mHxg = xg1, mAxg = xg2))

data

weights <-
  dials::grid_latin_hypercube(
  wmkt(),
  wxg(),
  wgoals(),
  xi(),
  size = 10
)

weights


