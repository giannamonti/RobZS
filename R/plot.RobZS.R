
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
         plotCoef.enetLTS0(x,vers=vers,...)
         plotResid.enetLTS0(x,vers=vers,...)
         plotDiagnostic.enetLTS0(x,vers=vers,...) }
      else if(method == "coefficients") plotCoef.enetLTS0(x,vers=vers,...)
      else if(method == "resid") plotResid.enetLTS0(x,vers=vers,...)
      else if(method == "diagnostic") plotDiagnostic.enetLTS0(x,vers=vers,...)

}
