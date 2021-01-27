
#' plotDiagnostic.RobZS
#'
#' @param object
#' @param vers
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotDiagnostic.RobZS <- function(object,vers=c("reweighted","raw"),...){

   vers <- match.arg(vers)
   family <- object$inputs$family

   y <- object$inputs$yy
   x <- object$inputs$xx

   if (isTRUE(object$inputs$intercept)){
      coefficients <- c(object$a0,object$coefficients)
      raw.coefficients <- c(object$a00,object$raw.coefficients)
   } else {
      coefficients <- object$coefficients
      raw.coefficients <- object$raw.coefficients
   }

   if (family=="binomial"){
      if (vers=="reweighted"){

         outind <- factor(as.numeric(object$raw.wt==0))
         xcoefficients <- x%*%coefficients
         names(xcoefficients) <- 1:length(xcoefficients)
         plot.xcoefficients <- data.frame(xcoefficients=xcoefficients,nam=names(xcoefficients),
                                          llim=xcoefficients,ulim=xcoefficients)
         xcoef.fit <- ggplot(plot.xcoefficients,aes(x=xcoefficients,y=y,color=outind,shape=outind)) + geom_point()
         xcoef.fit <- xcoef.fit + scale_colour_discrete(name="classification",breaks=c("0", "1"),labels=c("good","outlier")) +
            scale_shape_discrete(name="classification",breaks=c("0", "1"),labels=c("good","outlier")) +
            ggtitle(expression(paste("y vs ", X*beta, " for logistic regression"))) +
            xlab(expression(X*beta))
         print(xcoef.fit)

      } else if (vers=="raw"){

         classification <- factor(as.numeric(!c(1:length(y))%in%object$best))
         xraw.coefficients <- x%*%raw.coefficients
         names(xraw.coefficients) <- 1:length(xraw.coefficients)
         plot.xraw.coefficients <- data.frame(xraw.coefficients=xraw.coefficients,nam=names(xraw.coefficients),
                                              llim=xraw.coefficients,ulim=xraw.coefficients)
         raw.xcoef.fit <- ggplot(plot.xraw.coefficients,aes(x=xraw.coefficients,y=y,color=classification,shape=classification))+
            geom_point()
         raw.xcoef.fit <- raw.xcoef.fit + scale_colour_discrete(name="classification",breaks=c("0", "1"),labels=c("best subset","outlier")) +
            scale_shape_discrete(name="classification",breaks=c("0", "1"),labels=c("best subset","outlier")) +
            ggtitle(expression(paste("y vs ", X*beta, " with raw coefficients for logistic regression"))) + xlab(expression(X*beta))
         print(raw.xcoef.fit)
      }
   } else if (family=="gaussian"){
      if (vers=="reweighted"){

         outind <- factor(as.numeric(object$raw.wt==0))

         fitted.values <- x%*%coefficients
         names(fitted.values) <- 1:length(fitted.values)
         plotfitted.values <- data.frame(fitted.values=fitted.values,nam=names(fitted.values),llim=fitted.values,ulim=fitted.values)
         plot.fit <- ggplot(plotfitted.values, aes(x=fitted.values, y=y, color=outind, shape=outind)) +
            geom_point()
         plot.fit <- plot.fit + scale_colour_discrete(name="classification",breaks=c("0", "1"),labels=c("good","outlier")) +
            scale_shape_discrete(name="classification",breaks=c("0", "1"),labels=c("good","outlier")) +
            ggtitle("y vs fitted values for regression")
         print(plot.fit)

      } else if (vers=="raw"){

         raw.fitted.values <- x%*%raw.coefficients
         classification <- factor(as.numeric(!c(1:length(raw.fitted.values))%in%object$best))
         names(raw.fitted.values) <- 1:length(raw.fitted.values)
         raw.plotfitted.values <- data.frame(raw.fitted.values=raw.fitted.values,nam=names(raw.fitted.values),llim=raw.fitted.values,ulim=raw.fitted.values)
         raw.plot.fit <- ggplot(raw.plotfitted.values, aes(x=raw.fitted.values, y=y, color=classification, shape=classification)) +
            geom_point()
         raw.plot.fit <- raw.plot.fit + scale_colour_discrete(name="classification",breaks=c("0", "1"),labels=c("best subset","outlier")) +
            scale_shape_discrete(name="classification",breaks=c("0", "1"),labels=c("best subset","outlier")) +
            ggtitle("y vs raw fitted values for regression")
         print(raw.plot.fit)
      }
   }
}

