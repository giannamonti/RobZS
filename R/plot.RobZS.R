
#' plot.RobZS
#'
#' @param x
#' @param method
#' @param vers
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot.RobZS <- function(x,method=c("coefficients","resid","diagnostic"),
                         vers=c("reweighted","raw"),...){
   #  require(ggplot2)

   vers <- match.arg(vers)
   par(ask=T)
   readline(prompt = "Press <Enter> to continue...")
   ## call plot function

      if (missing("method")){
         plotCoef.RobZS(x,vers=vers,...)
         plotResid.RobZS(x,vers=vers,...)
         plotDiagnostic.RobZS(x,vers=vers,...) }
      else if(method == "coefficients") plotCoef.RobZS(x,vers=vers,...)
      else if(method == "resid") plotResid.RobZS(x,vers=vers,...)
      else if(method == "diagnostic") plotDiagnostic.RobZS(x,vers=vers,...)

}

