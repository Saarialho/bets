#' calculate kelly bet
#'
#' @param ev expected value
#' @param kerroin pelin kerroin
#' @param max_limit max bet
#' @export
kelly_bet <- function(ev, kerroin, max_limit){

  Tilinrahat <- 25000

  b = ((ev)/(kerroin-1))
  panos = b*Tilinrahat
  max_bet = min(0.02*Tilinrahat, 400)

  #pitaa hieman tarkkailla limitteja nyt 0.9x max_limit
  if(panos >= 0.9*max_limit){
    panos = 0.9*max_limit
  }

  if (b <= 0){
    panos = 0
  }

  nettoraja <- 1300/(kerroin-1)
  panos <- min(panos, nettoraja)
  panos <- min(panos, max_bet)

  round(panos, 2)
}
