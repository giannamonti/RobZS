
plotResid.enetLTS0 <- function(object,vers=c("reweighted","raw"),...){


   vers <- match.arg(vers)
   family <- object$inputs$family

   x <- object$inputs$xx
   y <- object$inputs$yy

   if (isTRUE(object$inputs$intercept)){
      coefficients <- c(object$a0,object$coefficients)
      raw.coefficients <- c(object$a00,object$raw.coefficients)
   } else {
      coefficients <- object$coefficients
      raw.coefficients <- object$raw.coefficients
   }

      if (vers=="reweighted"){
         outind <- factor(as.numeric(object$raw.wt==0))
         residuals <- y - x %*% coefficients
         fitted.values <- x %*% coefficients
         names(fitted.values) <- 1:length(fitted.values)
         plotfitted.values <- data.frame(fitted.values=fitted.values,nam=names(fitted.values),llim=fitted.values,ulim=fitted.values)
         plot.fit <- ggplot(plotfitted.values, aes(x=fitted.values, y=residuals, color=outind, shape=outind)) +
            geom_point()
         plot.fit <- plot.fit + scale_colour_discrete(name="classification",breaks=c("0", "1"),labels=c("good","outlier")) +
            scale_shape_discrete(name="classification",breaks=c("0", "1"),labels=c("good","outlier")) +
            ggtitle("Residuals vs fitted values for regression")
         print(plot.fit)
         #----------------------------
         classification <- factor(as.numeric(object$raw.wt==0))
         residuals <- object$residuals
         Index <- as.numeric(c(1:length(residuals)))
         names(residuals) <- 1:length(residuals)
         residuals <- data.frame(residuals=residuals,nam=names(residuals),
                                 llim=residuals,ulim=residuals)
         plot.resid <- ggplot(residuals, aes(x=Index, y=residuals, color=classification, shape=classification)) + geom_point()
         plot.resid <- plot.resid + scale_colour_discrete(name="classification",breaks=c("0", "1"),labels=c("good","outlier")) +
            scale_shape_discrete(name="classification",breaks=c("0", "1"),labels=c("good","outlier")) +
            ggtitle("Residuals vs indices for regression")
         print(plot.resid)
      }else if (vers=="raw"){
         raw.residuals <- y - x %*% raw.coefficients
         raw.fitted.values <- x %*% raw.coefficients
         classification <- factor(as.numeric(!c(1:length(raw.residuals))%in%object$best))

         names(raw.fitted.values) <- 1:length(raw.fitted.values)
         raw.plotfitted.values <- data.frame(raw.fitted.values=raw.fitted.values,nam=names(raw.fitted.values),llim=raw.fitted.values,ulim=raw.fitted.values)

         raw.plot.fit <- ggplot(raw.plotfitted.values, aes(x=raw.fitted.values, y=raw.residuals, color=classification, shape=classification)) +
            geom_point()
         raw.plot.fit <- raw.plot.fit + scale_colour_discrete(name="classification",breaks=c("0", "1"),labels=c("best subset","outlier")) +
            scale_shape_discrete(name="classification",breaks=c("0", "1"),labels=c("best subset","outlier")) +
            ggtitle("Raw residuals vs raw fitted values for regression")
         print(raw.plot.fit)
         #------------------------------
         Index <- as.numeric(c(1:length(raw.residuals)))
         names(raw.residuals) <- 1:length(raw.residuals)
         raw.residuals <- data.frame(raw.residuals=raw.residuals,nam=names(raw.residuals),
                                     llim=raw.residuals,ulim=raw.residuals)
         plot.raw.resid <- ggplot(raw.residuals, aes(x=Index, y=raw.residuals, color=classification, shape=classification)) +
            geom_point()
         plot.raw.resid <- plot.raw.resid + scale_colour_discrete(name="classification",breaks=c("0", "1"),labels=c("best subset","outlier")) +
            scale_shape_discrete(name="classification",breaks=c("0", "1"),labels=c("best subset","outlier")) +
            ggtitle("Raw residuals vs indices for regression")
         print(plot.raw.resid)
      }
}

