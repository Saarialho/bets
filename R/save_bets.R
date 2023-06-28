#' helper fun to save bets
#' @param to_save data to ...
#' @param arviot vai bets kumpi tallennetaan
#' @export
save_bets <- function(to_save, arviot = TRUE){

  if(arviot){

    hist_arviot <- bets::hist_arviot

    hist_arviot <- hist_arviot %>%
      bind_rows(to_save %>%
                  filter(!(id %in% hist_arviot$id), dts > 1) %>%
                  mutate(kohde = as.numeric(kohde)))

    use_data(hist_arviot, overwrite = TRUE)

  } else {

    hist_bets <- bets::hist_bets

    hist_bets <- hist_bets %>%
      bind_rows(to_save %>%
                  filter(!(id %in% hist_bets$id), dts > 1) %>%
                  mutate(kohde = as.numeric(kohde)))

    use_data(hist_bets, overwrite = TRUE)

  }
}

