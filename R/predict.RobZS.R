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
      if(type0=="class" & object$inputs$family!="binomial"){ stop("'class' is only available for logistic regression")}

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
               reweighted.nonzeroCoef=nonzeroCoef.RobZS(reweighted.coefficients)
               out.nonzero <- list(reweighted.nonzeroCoef=reweighted.nonzeroCoef)
            } else if (vers=="raw"){
               raw.nonzeroCoef=nonzeroCoef.RobZS(raw.coefficients)
               out.nonzero <- list(raw.nonzeroCoef=raw.nonzeroCoef)
            } else if (vers=="both"){
               reweighted.nonzeroCoef <- nonzeroCoef.RobZS(reweighted.coefficients)
               raw.nonzeroCoef <- nonzeroCoef.RobZS(raw.coefficients)
               out.nonzero <- list(reweighted.nonzeroCoef=reweighted.nonzeroCoef,
                                   raw.nonzeroCoef=raw.nonzeroCoef)
            }
            return(out.nonzero)
         }

         if (object$inputs$family=="binomial"){
            if (type0=="class"){

               if (vers=="reweighted"){
                  res <- newX%*%reweighted.coefficients
                  cnum <- ifelse(res>0.5,2,1)
                  reweighted.class <- object$classnames[cnum]
                  fit.class <- list(reweighted.class=reweighted.class)
               } else if (vers=="raw"){
                  res <- newX%*%raw.coefficients
                  cnum <- ifelse(res>0.5,2,1)
                  raw.class <- object$classnames[cnum]
                  fit.class <- list(raw.class=raw.class)
               } else if (vers=="both"){
                  res1 <- newX%*%reweighted.coefficients
                  cnum <- ifelse(res1>0.5,2,1)
                  reweighted.class=object$classnames[cnum]
                  res2 <- newX%*%raw.coefficients
                  cnum <- ifelse(res2>0.5,2,1)
                  raw.class <- object$classnames[cnum]
                  fit.class <- list(reweighted.class=reweighted.class,raw.class=raw.class)
               }
               return(fit.class)
            } else if (type0=="response"){

               if (vers=="reweighted"){
                  res <- 1/(1+exp(-newX%*%reweighted.coefficients))
                  fit.response <- list(reweighted.response=res)
               } else if (vers=="raw"){
                  res <- 1/(1+exp(-newX%*%raw.coefficients))
                  fit.response <- list(raw.response=res)
               } else if (vers=="both"){
                  res1 <- 1/(1+exp(-newX%*%reweighted.coefficients))
                  res2 <- 1/(1+exp(-newX%*%raw.coefficients))
                  fit.response <- list(reweighted.response=res1,raw.response=res2)
               }
               return(fit.response)
            }
         } else if (object$inputs$family=="gaussian"){
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
               reweighted.nonzeroCoef=nonzeroCoef.RobZS(reweighted.coefficients)
               out.nonzero <- list(reweighted.nonzeroCoef=reweighted.nonzeroCoef)
            } else if (vers=="raw"){
               raw.nonzeroCoef=nonzeroCoef.RobZS(raw.coefficients)
               out.nonzero <- list(raw.nonzeroCoef=raw.nonzeroCoef)
            } else if (vers=="both"){
               reweighted.nonzeroCoef=nonzeroCoef.RobZS(reweighted.coefficients)
               raw.nonzeroCoef=nonzeroCoef.RobZS(raw.coefficients)
               out.nonzero <- list(reweighted.nonzeroCoef=reweighted.nonzeroCoef,
                                   raw.nonzeroCoef=raw.nonzeroCoef)
            }
            return(out.nonzero)
         }

         if (object$inputs$family=="binomial"){
            if (type0=="class"){

               if (vers=="reweighted"){
                  res <- newX%*%reweighted.coefficients
                  cnum <- ifelse(res>0.5,2,1)
                  reweighted.class <- object$classnames[cnum]
                  fit.class <- list(reweighted.class=reweighted.class)
               } else if (vers=="raw"){
                  res <- newX%*%raw.coefficients
                  cnum <- ifelse(res>0.5,2,1)
                  raw.class <- object$classnames[cnum]
                  fit.class <- list(raw.class=raw.class)
               } else if (vers=="both"){
                  res1 <- newX%*%reweighted.coefficients
                  cnum <- ifelse(res1>0.5,2,1)
                  reweighted.class=object$classnames[cnum]
                  res2 <- newX%*%raw.coefficients
                  cnum <- ifelse(res2>0.5,2,1)
                  raw.class <- object$classnames[cnum]
                  fit.class <- list(reweighted.class=reweighted.class,raw.class=raw.class)
               }
               return(fit.class)
            } else if (type0=="response"){

               if (vers=="reweighted"){
                  res <- 1/(1+exp(-newX%*%reweighted.coefficients))
                  fit.response <- list(reweighted.response=res)
               } else if (vers=="raw"){
                  res <- 1/(1+exp(-newX%*%raw.coefficients))
                  fit.response <- list(raw.response=res)
               } else if (vers=="both"){
                  res1 <- 1/(1+exp(-newX%*%reweighted.coefficients))
                  res2 <- 1/(1+exp(-newX%*%raw.coefficients))
                  fit.response <- list(reweighted.response=res1,raw.response=res2)
               }
               return(fit.response)
            }
         } else if (object$inputs$family=="gaussian"){
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
   }


