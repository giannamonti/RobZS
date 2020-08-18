
fitted.enetLTS0 <-
   function(object,vers=c("reweighted","raw","both"),type0=c("response"),...){

      vers <- match.arg(vers)
      type0 <- match.arg(type0)

      if (object$inputs$intercept==TRUE){
         reweighted.coefficients <- c(object$a0,object$coefficients)
         raw.coefficients <- c(object$a00,object$raw.coefficients)
      } else {
         reweighted.coefficients <- c(object$a0,object$coefficients)
         raw.coefficients <- c(object$a00,object$raw.coefficients)
      }


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

      return(nfit)
   }



