#rset helpers
#' @export extract_dates_rsplit
extract_dates_rsplit <- function(rsplit_object, date_col_name = "date"){
  an_obj <- rsample::analysis(rsplit_object)
  as_obj <- rsample::assessment(rsplit_object)

  tibble(analysis_min = min(an_obj[[date_col_name]]),
         analysis_max = max(an_obj[[date_col_name]]),
         assessment_min = min(as_obj[[date_col_name]]),
         assessment_max = max(as_obj[[date_col_name]])
  )
}

#' @export extract_dates_rset
extract_dates_rset <- function(rset_object, date_col_name = "date"){
  rset_object %>%
    mutate(dates = purrr::map(splits, extract_dates_rsplit, date_col_name = date_col_name)) %>% #jos mutate. niin ei riita memory
    tidyr::unnest(dates)
}

#' @export plot_dates_rset
plot_dates_rset <- function(rset_object_with_dates){

  rset_object_with_dates %>%
    dplyr::select(-one_of("splits")) %>%
    tidyr::pivot_longer(cols = where(lubridate::is.Date),
                        names_to = "type",
                        values_to = "date") %>%
    separate(type, c("data_type", "range_type"), "_") %>%
    ggplot(aes(y = forcats::fct_rev(id), x = date, colour = forcats::fct_rev(data_type)))+
    geom_line(size = 2)+
    theme_minimal()+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}
