
#' coef.RobZS
#'
#' @param object fitted RobZS model object.
#' @param vers a character string specifying for which fit to make predictions. Possible values are reweighted (the default) for predicting values from the reweighted fit, raw for predicting values from the raw fit.
#' @param zeros a logical indicating whether to keep zero coefficients ('TRUE', the default) or to omit them ('FALSE').
#' @param ... additional arguments from the RobZS object if needed.
#'
#' @return a numeric vector containing the requested coefficients.
#' @export
#'
#' @examples
coef.RobZS <-
   function(object ,vers=c("reweighted","raw"), zeros=TRUE,...)
   {
      vers=match.arg(vers)
      nbeta <- predict.RobZS(object,newX=object$inputs$xx,vers=vers,type0="coefficients",...)
      nbeta <- as.numeric(unlist(nbeta))
      if (isTRUE(zeros)) {
         nbeta <- nbeta
         names(nbeta) <- 1:length(nbeta)
      } else if (!isTRUE(zeros)) {
         namesbeta <- which(nbeta != 0)
         nbeta <- nbeta[nbeta != 0]
         names(nbeta) <- namesbeta
      }
      return(nbeta)
   }


