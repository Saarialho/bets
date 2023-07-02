
#' helper fun to fit models
#' @param pin_data data to fit
#' @param configs lasso configs
#' @param min_games kuinka paljon minimissaan peleja
#' @export
fit_models <- function(pin_data, configs, min_games = 60){
  pin_names %>%
    dplyr::group_nest(league) %>%
    dplyr::filter(purrr::map_dbl(data, nrow) >= min_games) %>% #min amount of games played
    tidyr::unnest(data) %>%
    dplyr::mutate(configs = list(configs %>% select(-penalty))) %>%
    tidyr::unnest(c(configs)) %>%
    dplyr::mutate(hwxg = wmkt*mhxg + wxg*h_xg + wgoals*FTHG,
                      awxg = wmkt*maxg + wxg*a_xg + wgoals*FTAG) %>%
    dplyr::group_nest(league, model_id, xi) %>%
    dplyr::mutate(fit = purrr::map2(data, xi, fit_multimixture_model)) %>%
    dplyr::select(league, model_id, fit) %>%
    dplyr::mutate(teams = map(fit, ~.x %>% purrr::pluck('all_teams')))
}
