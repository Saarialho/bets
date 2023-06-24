#' log in to pinnacle
#'
#' @param account account name
#' @export
log_in_pinnacle <- function(account = 'ES1089850'){
  pinnacle.API::AcceptTermsAndConditions(accepted=TRUE)
  pinnacle.API::SetCredentials(account, Sys.getenv('PSWRD'))
}
