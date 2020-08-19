#' residuals.RobZS
#'
#' @param object
#' @param vers
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
residuals.RobZS <-
   function(object,vers=c("reweighted","raw","both"),...){

      vers <- match.arg(vers)

      if (vers=="reweighted"){
            residuals <- object$inputs$y - object$inputs$x %*% c(object$a0,object$coefficients)
         nfit <- list(residuals=residuals)
      } else if (vers=="raw"){
            raw.residuals <- object$inputs$y - object$inputs$x %*% c(object$a0,object$raw.coefficients)
         nfit <- list(raw.residuals=raw.residuals)
      } else if (vers=="both"){
            residuals <- object$inputs$y - object$inputs$x %*% c(object$a0,object$coefficients)
            raw.residuals <- object$inputs$y - object$inputs$x %*% c(object$a0,object$raw.coefficients)
         nfit <- list(residuals=residuals,raw.residuals=raw.residuals)
      }
      nfit
   }
