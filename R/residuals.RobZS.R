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
         if (object$inputs$family=="binomial"){
            residuals <-  -(object$inputs$y * object$inputs$x %*% c(object$a0,object$coefficients)) +
               log(1+exp(object$inputs$x %*% c(object$a0,object$coefficients)))
         } else if (object$inputs$family=="gaussian"){
            residuals <- object$inputs$y - object$inputs$x %*% c(object$a0,object$coefficients)
         }
         nfit <- list(residuals=residuals)
      } else if (vers=="raw"){
         if (object$inputs$family=="binomial"){
            raw.residuals <- -(object$inputs$y * object$inputs$x %*% c(object$a0,object$raw.coefficients)) +
               log(1+exp(object$inputs$x %*% c(object$a0,object$raw.coefficients)))
         } else if (object$inputs$family=="gaussian"){
            raw.residuals <- object$inputs$y - object$inputs$x %*% c(object$a0,object$raw.coefficients)
         }
         nfit <- list(raw.residuals=raw.residuals)
      } else if (vers=="both"){
         if (object$inputs$family=="binomial"){
            residuals <-  -(object$inputs$y * object$inputs$x %*% c(object$a0,object$coefficients)) +
               log(1+exp(object$inputs$x %*% c(object$a0,object$coefficients)))
            raw.residuals <- -(object$inputs$y * object$inputs$x %*% c(object$a0,object$raw.coefficients)) +
               log(1+exp(object$inputs$x %*% c(object$a0,object$raw.coefficients)))
         } else if (object$inputs$family=="gaussian"){
            residuals <- object$inputs$y - object$inputs$x %*% c(object$a0,object$coefficients)
            raw.residuals <- object$inputs$y - object$inputs$x %*% c(object$a0,object$raw.coefficients)
         }
         nfit <- list(residuals=residuals,raw.residuals=raw.residuals)
      }
      nfit
   }
