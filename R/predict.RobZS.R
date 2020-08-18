

#' predict.RobZS
#'
#' @param object
#' @param newX
#' @param vers
#' @param type0
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
predict.RobZS <-
   function(object,newX,vers=c("reweighted","raw","both"),
            type0=c("response","coefficients","nonzero"),...)
   {

      type0 <- match.arg(type0)
      vers <- match.arg(vers)

      if(missing(newX)){
         if(!match(type0,c("coefficients","nonzero"),FALSE)) stop("You need to supply a value for 'newX'")
      }

      if(vers=="reweighted"){
         reweighted.coefficients <- object$coefficients
      } else if (vers=="raw"){
         raw.coefficients <- object$raw.coefficients
      } else if(vers=="both"){
         reweighted.coefficients <- object$coefficients
         raw.coefficients <- object$raw.coefficients
      }

      if (object$inputs$intercept=="TRUE"){  #### intercept
         reweighted.coefficients <- c(object$a0,object$coefficients)
         raw.coefficients <- c(object$a00,object$raw.coefficients)
         newX <- cbind(1,newX)

         if (vers=="reweighted"){
            out <- list(reweighted.coefficients=reweighted.coefficients)
         } else if (vers=="raw"){
            out <- list(raw.coefficients=raw.coefficients)
         } else if (vers=="both"){
            out <- list(reweighted.coefficients=reweighted.coefficients,raw.coefficients=raw.coefficients)
         }
         if (type0=="coefficients") return(out)
         if (type0=="nonzero"){
            if (vers=="reweighted"){
               reweighted.nonzeroCoef=nonzeroCoef.enetLTS0(reweighted.coefficients)
               out.nonzero <- list(reweighted.nonzeroCoef=reweighted.nonzeroCoef)
            } else if (vers=="raw"){
               raw.nonzeroCoef=nonzeroCoef.enetLTS0(raw.coefficients)
               out.nonzero <- list(raw.nonzeroCoef=raw.nonzeroCoef)
            } else if (vers=="both"){
               reweighted.nonzeroCoef <- nonzeroCoef.enetLTS0(reweighted.coefficients)
               raw.nonzeroCoef <- nonzeroCoef.enetLTS0(raw.coefficients)
               out.nonzero <- list(reweighted.nonzeroCoef=reweighted.nonzeroCoef,raw.nonzeroCoef=raw.nonzeroCoef)
            }
            return(out.nonzero)
         }
            if (vers=="reweighted"){
               res=as.matrix(newX%*%reweighted.coefficients)
               fit.response <- list(reweighted.response=res)
            } else if (vers=="raw"){
               res=as.matrix(newX%*%raw.coefficients)
               fit.response <- list(raw.response=res)
            } else if (vers=="both"){
               res1=as.matrix(newX%*%reweighted.coefficients)
               res2=as.matrix(newX%*%raw.coefficients)
               fit.response <- list(reweighted.response=res1,raw.response=res2)
            }
            return(fit.response)
      } else {
         if (vers=="reweighted"){
            out <- list(reweighted.coefficients=reweighted.coefficients)
         } else if (vers=="raw"){
            out <- list(raw.coefficients=raw.coefficients)
         } else if (vers=="both"){
            out <- list(reweighted.coefficients=reweighted.coefficients,raw.coefficients=raw.coefficients)
         }
         if (type0=="coefficients") return(out)
         if (type0=="nonzero"){
            if (vers=="reweighted"){
               reweighted.nonzeroCoef=nonzeroCoef.enetLTS0(reweighted.coefficients)
               out.nonzero <- list(reweighted.nonzeroCoef=reweighted.nonzeroCoef)
            } else if (vers=="raw"){
               raw.nonzeroCoef=nonzeroCoef.enetLTS0(raw.coefficients)
               out.nonzero <- list(raw.nonzeroCoef=raw.nonzeroCoef)
            } else if (vers=="both"){
               reweighted.nonzeroCoef=nonzeroCoef.enetLTS0(reweighted.coefficients)
               raw.nonzeroCoef=nonzeroCoef.enetLTS0(raw.coefficients)
               out.nonzero <- list(reweighted.nonzeroCoef=reweighted.nonzeroCoef,raw.nonzeroCoef=raw.nonzeroCoef)
            }
            return(out.nonzero)
         }
            if (vers=="reweighted"){
               res=as.matrix(newX%*%reweighted.coefficients)
               fit.response <- list(reweighted.response=res)
            } else if (vers=="raw"){
               res=as.matrix(newX%*%raw.coefficients)
               fit.response <- list(raw.response=res)
            } else if (vers=="both"){
               res1=as.matrix(newX%*%reweighted.coefficients)
               res2=as.matrix(newX%*%raw.coefficients)
               fit.response <- list(reweighted.response=res1,raw.response=res2)
            }
            return(fit.response)

      }
   }


