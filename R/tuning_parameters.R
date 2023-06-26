#' parameters for modeling

#' @export
wmkt <- function(range = c(0, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(wmkt = "weight of market xg"),
    finalize = NULL
  )
}

#' @export
wxg <- function(range = c(0, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(wxg = "weight of xg"),
    finalize = NULL
  )
}

#' @export
wgoals <- function(range = c(0, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(wgoals = "weight of goals"),
    finalize = NULL
  )
}

#' @export
xi <- function(range = c(0, 0.15), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(xi = "weight of time"),
    finalize = NULL
  )
}


