
coef.enetLTS0 <-
   function(object ,vers=c("reweighted","raw"), zeros=TRUE,...)
   {
      vers=match.arg(vers)
      nbeta <- predict.enetLTS0(object,newX=object$inputs$xx,vers=vers,type0="coefficients",...)
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


