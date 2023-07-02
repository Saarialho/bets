#' helper fun to save bets
#' @param to_save data to ...
#' @param arviot vai bets kumpi tallennetaan
#' @param totals logical
#' @export
save_bets <- function(to_save, arviot = TRUE, totals = TRUE){

  path_dic <-
    tibble::tibble(paths = c('multimodel_arviot.rds', 'multimodel_bets.rds', 'totals_arviot.rds', 'totals_bets.rds'),
           totals_lgl = c(FALSE, FALSE, TRUE, TRUE),
           arviot_lgl = c(TRUE, FALSE, TRUE, FALSE))

  file_end <- path_dic %>%
    dplyr::filter(totals_lgl == totals & arviot_lgl == arviot) %>%
    dplyr::pull(paths)

  path <- file.path("~/Documents/bets/output", file_end)
  old_data <- qs::qread(path)

  updated <- old_data %>%
    dplyr::mutate(kohde = as.numeric(kohde)) %>%
    dplyr::bind_rows(to_save %>%
                       dplyr::filter(!(id %in% old_data$id), dts > 1) %>%
                       dplyr::mutate(kohde = as.numeric(kohde)))

  updated %>% qs::qsave(path)

}

path <- file.path("~/Documents/bets/output", 'totals_arviot.rds')
old_data <- qs::qread(path)

