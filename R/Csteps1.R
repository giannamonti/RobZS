
beginningCstep0 <- function(x,y,family,h,hsize,alpha,lambda,nsamp,s1,ncores,csteps,tol,intercept,scal,para,seed){
   ## internal function for Cstep0 and warmCsteps0

   #  source("objectiveFunc0.R")
   #  source("InitialSubsets0.R")
   #  source("Csteps0.R")
   #  source("utilities.R")

   H2 <- selectbest10zero(x,y,family,h,hsize,alpha,lambda,nsamp,s1,para,ncores,intercept,scal,seed)
   if (para){
      lastbestindex <- mclapply(1:s1, function(zz,x,y,family,h,hsize,alpha,lambda,H2) {
         indexsubbest <- H2$idxbest[[zz]]
         objbest <- tol
         cstep.mod <- CStep0(x,y,family,indexsubbest,h,hsize,alpha,lambda,intercept,scal)
         countloop <- 0
         while ((cstep.mod$object>objbest) & (countloop<csteps)){
            countloop <- countloop+1
            objbest <- cstep.mod$object
            newindex <- cstep.mod$index
            beta <- cstep.mod$beta
            cstep.mod <- CStep0(x,y,family,newindex,h,hsize,alpha,lambda,intercept,scal)
         }
         return(list(lastindex=newindex,objbest=objbest,countloop=countloop,
                     residu=cstep.mod$residu,beta=beta))
      },x,y,family,h,hsize,alpha,lambda,H2,mc.cores = ncores)
   }else{ # not parallel
      lastbestindex <- lapply(1:s1, function(zz,x,y,family,h,hsize,alpha,lambda,H2) {
         indexsubbest <- H2$idxbest[[zz]]
         objbest <- tol
         cstep.mod <- CStep0(x,y,family,indexsubbest,h,hsize,alpha,lambda,intercept,scal)
         countloop <- 0
         while ((cstep.mod$object>objbest) & (countloop<csteps)){
            countloop <- countloop+1
            objbest <- cstep.mod$object
            newindex <- cstep.mod$index
            beta <- cstep.mod$beta
            cstep.mod <- CStep0(x,y,family,newindex,h,hsize,alpha,lambda,intercept,scal)
         }
         return(list(lastindex=newindex,objbest=objbest,countloop=countloop,
                     residu=cstep.mod$residu,beta=beta))
      },x,y,family,h,hsize,alpha,lambda,H2)
   }
   obj <- NULL
   for (i in 1:s1){
      obj <- c(obj,lastbestindex[[i]]$objbest)
   }
   whichbestindex <- sort(obj,decreasing=TRUE,index.return=TRUE)$ix[1]
   index <- lastbestindex[[whichbestindex]]$lastindex
   resid <- lastbestindex[[whichbestindex]]$residu
   return(list(index=index,resid=drop(resid)))
}


CStep0 <-
   function(x,y,family,indx,h,hsize,alpha,lambda,intercept,scal)
   {
      ## internal function

      # require(glmnet)
      # source("utilities.R")
      # source("objectiveFunc1.R")

      n <- nrow(x)

      if (family=="binomial"){
         fit <- zeroSum(x[indx,],y[indx],family,alpha=alpha,lambda=lambda,
                        standardize=scal, intercept=intercept)
         beta <- fit$coef[[1]]
         beta <- matrix(beta[-1])
         resid <- -(y * x %*% beta) + log(1+exp(x %*% beta))
         if(all(beta==0)){return(list(object=-Inf,index=indx,residu=resid,beta=beta))}
         resid.sort <- sort(resid,decreasing=FALSE,index.return=TRUE)
         h0 <- floor((length(y[y==0])+1)*hsize)
         h1 <- h-h0
         index0 <- resid.sort$ix[y[resid.sort$ix]==0][1:h0]
         index1 <- resid.sort$ix[y[resid.sort$ix]==1][1:h1]
         indxnew <- c(index0,index1)
      }else if(family=="gaussian"){
         fit <- zeroSum(x[indx,],y[indx],family,
                        alpha=alpha,lambda=lambda,
                        standardize=scal, intercept=intercept)
         beta <- fit$coef[[1]]
         beta <- matrix(beta[-1])
         resid <- y - predict(fit,x,exact=TRUE)
         resid.sort <- sort(abs(resid),index.return=TRUE)
         indxnew <- resid.sort$ix[1:h]
      }
      obj <- Objval0(x,y,family,beta,indxnew,alpha,lambda)

      return(list(object=obj,index=indxnew,residu=resid,beta=beta))
   }







