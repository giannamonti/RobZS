#' print.RobZS
#'
#' @param x
#' @param vers
#' @param zeros
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.RobZS <-
   function(x,vers=c("reweighted","raw"),zeros=FALSE,...){

      # require(RobZS)
      # require(predict.RobZS)
      # require(coef.RobZS)

      vers <- match.arg(vers)
      cat("RobZS estimator \n")

      cat("\nCall: ", deparse(x$call), "\n\n")

      coefficients <- coef.RobZS(x,vers=vers,zeros=zeros)
      cat("\nCoefficients:\n")
      print(coefficients,...)

      nCoefficients <- sum(coefficients!=0)
      cat("\n number of the nonzero coefficients:\n")
      print(nCoefficients)

      cat(paste("\n alpha:",x$alpha))
      cat(paste("\n lambda:",x$lambda))
      cat(paste("\n lambdaw:",x$lambdaw))

}
