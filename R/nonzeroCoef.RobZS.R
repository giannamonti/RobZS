
#' nonzeroCoef.RobZS
#'
#' @param beta
#'
#' @return
#' @export
#'
#' @examples
nonzeroCoef.RobZS  <- function (beta)
{
  beta <- as.matrix(beta)
  beta <- abs(beta)>0
  beta <- which(beta)
  names(beta) <- 1:length(beta)
  return(beta)
}

