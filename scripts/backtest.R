

data <- join_buch_fbref(bets::hist_buch_data, bets::fbref_data)

data %>% skimr::skim()

problems <- data %>%
  group_by(home) %>%
  summarise(count_na = sum(is.na(h_xg))) %>%
  arrange(desc(count_na)) %>%
  filter(count_na > 5)
