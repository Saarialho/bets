
hist_buch_data

fbref_data


hist_buch_data %>%
  distinct(home)

fbref_data <- fbref_data %>%
  distinct(home) %>%
  rename(fbref_home = home)


fuzzy_join <- hist_buch_data %>%
  distinct(home) %>%
  fuzzyjoin::stringdist_left_join(fbref_data, by = c('home' = 'fbref_home'), max_dist = 4) %>%
  mutate(correct = if_else(home == fbref_home, TRUE, FALSE))

problem_names <- fuzzy_join %>%
  group_by(home) %>%
  count(correct) %>%
  mutate(any_correct = if_else(any(correct == TRUE), TRUE, FALSE)) %>%
  mutate(any_correct = replace_na(any_correct, FALSE)) %>%
  ungroup() %>%
  filter(any_correct == FALSE)


fuzzy_join %>%
  filter(home %in% problem_names$home) %>% View()
