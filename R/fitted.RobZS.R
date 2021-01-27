
#' fitted.RobZS
#'
#' @param object
#' @param vers
#' @param type0
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fitted.RobZS <-
   function(object,vers=c("reweighted","raw","both"),type0=c("response"),...){

      vers <- match.arg(vers)
      type0 <- match.arg(type0)

      if(type0=="class" & object$inputs$family!="binomial"){
         stop("'class' is only available for logistic regression")}

      if (object$inputs$intercept==TRUE){
         reweighted.coefficients <- c(object$a0,object$coefficients)
         raw.coefficients <- c(object$a00,object$raw.coefficients)
      } else {
         reweighted.coefficients <- c(object$a0,object$coefficients)
         raw.coefficients <- c(object$a00,object$raw.coefficients)
      }

      if (object$inputs$family=="binomial"){
         if (vers=="reweighted"){
            u <- object$inputs$x %*% reweighted.coefficients
            if (type0=="class"){
               fitted.values <- ifelse(u>0.5,0,1)
            } else if (type0=="response"){
               fitted.values <-  1/(1+exp(-u))
            }
            nfit <- list(fitted.values=fitted.values)
         } else if (vers=="raw"){
            uu <- object$inputs$x %*% raw.coefficients
            if (type0=="class"){
               raw.fitted.values <- ifelse(uu>0.5,0,1)
            } else if (type0=="response"){
               raw.fitted.values <-  1/(1+exp(-uu))
            }
            nfit <- list(raw.fitted.values=raw.fitted.values)
         } else if (vers=="both"){
            u <- object$inputs$x %*% reweighted.coefficients
            uu <- object$inputs$x %*% reweighted.coefficients
            if (type0=="class"){
               fitted.values <- ifelse(u>0.5,0,1)
            } else if (type0=="response"){
               fitted.values <- 1/(1+exp(-u))
            }
            if (type0=="class"){
               raw.fitted.values <- ifelse(uu>0.5,0,1)
            } else if (type0=="response"){
               raw.fitted.values <- 1/(1+exp(-uu))
            }
            nfit <- list(fitted.values=fitted.values,raw.fitted.values=raw.fitted.values)
         }
      } else if (object$inputs$family=="gaussian"){
         if (vers=="reweighted"){
            res=as.matrix(object$inputs$x %*% reweighted.coefficients)
            nfit <- list(fitted.values=res)
         } else if (vers=="raw"){
            res=as.matrix(object$inputs$x %*% raw.coefficients)
            nfit <- list(raw.fitted.values=res)
         } else if (vers=="both"){
            res1=as.matrix(object$inputs$x %*% reweighted.coefficients)
            res2=as.matrix(object$inputs$x %*% raw.coefficients)
            nfit <- list(fitted.values=res1,raw.fitted.values=res2)

         }

      }

      return(nfit)
   }



