# add speed values
#' calculate stdErrXY
#'
#' TODO...
#'
#' @param varX X varance  vector
#' @param varY Y vriance vector
#' @param covXY common variance vector 
#'
#' @return vector of formula for accuracy
#'
#' @examples
#' TODO
#' @export

stdevFilt <- function(varX,varY,covXY) {
  t <- varX + varY
  d <- varX*varY - (covXY^2)
  e <- t/2 + sqrt((t^2)/4 + d)
  return(sqrt(e))
}