#' helper fun to save bets
#' @param to_save data to ...
#' @param arviot vai bets kumpi tallennetaan
#' @export
save_bets <- function(to_save, arviot = TRUE){

  if(arviot){

    hist_arviot <- qs::qread("~/Documents/bets/output/multimodel_arviot.rds") %>%
      dplyr::bind_rows(to_save %>%
                         dplyr::filter(!(id %in% hist_arviot$id), dts > 1) %>%
                         dplyr::mutate(kohde = as.numeric(kohde)))

    hist_arviot %>% qs::qsave("~/Documents/bets/output/multimodel_arviot.rds")

  } else {

    hist_bets <- qs::qread("~/Documents/bets/output/multimodel_bets.rds") %>%
      dplyr::bind_rows(to_save %>%
                         dplyr::filter(!(id %in% hist_bets$id), dts > 1) %>%
                         dplyr::mutate(kohde = as.numeric(kohde)))

    hist_bets %>% qs::qsave("~/Documents/bets/output/multimodel_bets.rds")

  }
}

