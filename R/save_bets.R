#' helper fun to save bets
#' @param to_save data to ...
#' @param arviot vai bets kumpi tallennetaan
#' @export
save_bets <- function(to_save, arviot = TRUE){

  if(arviot){

    hist_arviot <- qs::qread("~/Documents/bets/output/multimodel_arviot.rds")

    updated <- hist_arviot %>%
      dplyr::bind_rows(to_save %>%
                         dplyr::filter(!(id %in% hist_arviot$id), dts > 1) %>%
                         dplyr::mutate(kohde = as.numeric(kohde)))

    updated %>% qs::qsave("~/Documents/bets/output/multimodel_arviot.rds")

  } else {

    hist_bets <- qs::qread("~/Documents/bets/output/multimodel_bets.rds")

    updated <- hist_bets %>%
      dplyr::bind_rows(to_save %>%
                         dplyr::filter(!(id %in% hist_bets$id), dts > 1) %>%
                         dplyr::mutate(kohde = as.numeric(kohde)))

    updated %>% qs::qsave("~/Documents/bets/output/multimodel_bets.rds")

  }
}

