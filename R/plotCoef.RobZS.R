
#' plotCoef.RobZS
#'
#' @param object
#' @param vers
#' @param colors
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotCoef.RobZS <- function(object,vers=c("reweighted","raw"),
                           colors=NULL,...){

   nam <- NULL

   if(is.null(colors)){
      colors <- list(bars="#0000AA",errorbars="red",
                     background="#BBBBEE",abline="#21A0D2",
                     scores="#0000AA",cutoffs="#00EEEE",
                     badouts="darkred", modouts="black")
   }
   family <- object$inputs$family
   vers <- match.arg(vers)

   if (isTRUE(object$inputs$intercept)){
      coefficients <- c(object$a0,object$coefficients)
      raw.coefficients <- c(object$a00,object$raw.coefficients)
      names(coefficients)=c("Intercept",seq(from=1, to=(length(coefficients)-1)))
      names(raw.coefficients)=c("Intercept",seq(from=1, to=(length(coefficients)-1)))
   } else {
      coefficients <- object$coefficients
      raw.coefficients <- object$raw.coefficients
      names(coefficients)=seq(from=1, to=length(coefficients))
      names(raw.coefficients)=seq(from=1, to=length(coefficients))
   }

   if (vers=="reweighted"){


      plotcoefs <- data.frame(coefficients=coefficients,nam=names(coefficients),
                              llim=coefficients,ulim=coefficients)
      plotcoefs$nam <- factor(plotcoefs$nam, levels=names(coefficients))

      if (family=="binomial"){
         plot <- ggplot(plotcoefs,aes(nam,coefficients))+
            geom_bar(stat="identity",size=3,fill=colors$bars,position="identity")+
            labs(title=paste(names(object$inputs$yy),"RobZS coefficients for logistic regression"))
      } else if (family=="gaussian"){
         plot <- ggplot(plotcoefs,aes(nam,coefficients))+geom_bar(stat="identity",size=3,fill=colors$bars,position="identity")+
            labs(title=paste(names(object$inputs$yy),"RobZS coefficients for regression"))
      }


      plot <- plot + theme(panel.background=element_rect(fill=colors$background),
                           plot.title=element_text(size=rel(1),face="bold"),
                           axis.text.x=element_text(angle=-90),axis.title.x=element_blank(),
                           axis.title.y=element_blank())
      print(plot)
   } else if (vers=="raw"){

      raw.plotcoefs <- data.frame(raw.coefficients=raw.coefficients,nam=names(raw.coefficients),
                                  llim=raw.coefficients,ulim=raw.coefficients)
      raw.plotcoefs$nam <- factor(raw.plotcoefs$nam, levels=names(raw.coefficients))

      if (family=="binomial"){
         raw.plot <- ggplot(raw.plotcoefs,aes(nam,raw.coefficients))+geom_bar(stat="identity",size=3,fill=colors$bars,position="identity")+
            labs(title=paste(names(object$inputs$yy),"RobZS raw coefficients for logistic regression"))
      } else if (family=="gaussian"){
         raw.plot <- ggplot(raw.plotcoefs,aes(nam,raw.coefficients))+geom_bar(stat="identity",size=3,fill=colors$bars,position="identity")+
            labs(title=paste(names(object$inputs$yy),"RobZS raw coefficients for regression"))
      }

      raw.plot <- raw.plot + theme(panel.background=element_rect(fill=colors$background),
                                   plot.title=element_text(size=rel(1),face="bold"),
                                   axis.text.x=element_text(angle=-90),axis.title.x=element_blank(),
                                   axis.title.y=element_blank())
      print(raw.plot)
   }
}
