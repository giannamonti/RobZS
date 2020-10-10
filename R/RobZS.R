# Generate lambda sequence from zeroSum: Pf, July 10, 2020

RobZS <-
function(xx,yy,family=c("gaussian", "binomial"),
         alphas,lambdas,lambdaw, hsize=0.75,
				 intercept=TRUE,
         nsamp=500,s1=10,
         nCsteps=20,nfold=5,
         seed=NULL,plot=TRUE,
         repl=5,para=FALSE,ncores=1,
         del=0.0125,tol=-1e6,
         type0=c("response","class"))
{
   matchedCall <- match.call()
   matchedCall[[1]] <- as.name("RobZS")
   family <- match.arg(family)
   type0 <- match.arg(type0)

   xx <- addColnames(as.matrix(xx))
   nc <- dim(yy)
   if (is.null(nc)){
      yy <- as.matrix(yy)
   }

   n <- nrow(xx)
   p <- ncol(xx)
   h <- floor((n+1)*hsize)

   if (repl<=0) stop("repl has to be a positive number")
   if (nCsteps<=0) stop("nCsteps has to be a positive number")
   if (type0=="class" & family=="gaussian") stop("class type is not available for gaussian family")

   ncores <- rep(ncores, length.out=1)
   if(is.na(ncores)) ncores <- detectCores()  # use all available cores
   if(!is.numeric(ncores) || is.infinite(ncores) || ncores < 1) {
      ncores <- 1  # use default value
      warning("invalid value of 'ncores'; using default value")
   }
   else ncores <- as.integer(ncores)

   if (missing(alphas)){
      alphas <- seq(0,1,length=41)
   }
   alphas <- sort(alphas)
   wh <- (alphas<0 | alphas>1)
   if (sum(wh)>0) stop("alphas can take the values only between 0 and 1")


     sc <- prepara0(xx,yy,family,robu=1)
     x <- scale(xx,sc$mux,FALSE)
     y <- sc$ycen

   if (missing(lambdas)){
     ### l0 <- robustHD::lambda0(xx,yy,normalize=scal,intercept=intercept)
     ### lambdas <- seq(l0,0,by=-0.025*l0)
     lambdas <- zeroSum(x,y,family,alpha=alphas,standardize=FALSE,intercept=FALSE,lambdaSteps = 40)$lambda
   }

   WarmCstepresults <- warmCsteps0(x,y,h,n,p,family,alphas,lambdas,hsize,
                                 nsamp,s1,nCsteps,nfold,para,ncores,tol,seed)
   indexall <- WarmCstepresults$indexall

   if ((length(alphas)==1) & (length(lambdas)==1)){
      if (plot==TRUE) warning("There is no meaning to see plot for a single combination of lambda and alpha")
      indexbest <- drop(indexall)
      alphabest <- alphas
      lambdabest <- lambdas
   } else {
      CVresults <- cv.RobZS(indexall,x,y,family,h,alphas,lambdas,nfold,repl,ncores,plot)
      indexbest <- CVresults$indexbest
      alphabest <- CVresults$alphaopt
      lambdabest <- CVresults$lambdaopt
      evalCritCV <- CVresults$evalCrit
      minlambdaCrit <- CVresults$minlambdaCrit
   }
   #--------------------------------------------------------------------------------
   ### Change PF: lambdabest is based on fewer observations of training data -> not comparable!
   fit <- zeroSum(x[indexbest,],y[indexbest,],family,alpha=alphabest,lambda=minlambdaCrit, #### /h
                  standardize=FALSE,intercept=FALSE,type.measure="mse")
   ###fit1 <- zeroSum(x[indexbest,],y[indexbest,],family,alpha=alphabest,lambda=rev(lambdas), #### /h
   ###            standardize=FALSE,intercept=FALSE,type.measure="mse")
   ###fit <- zeroSum(x[indexbest,],y[indexbest,],family,alpha=alphabest,nFold=5,
   ###               standardize=FALSE,intercept=FALSE)
   ###beta <- fit$coef[[1]]
   beta <- as.vector(coef(fit,"lambda.min"))

   # final reweighting:
   ### a00 <- if (intercept==FALSE) 0 else drop(sc$muy+beta[1]-as.vector(as.matrix(beta[-1]))%*%(sc$mux/sc$sigx))
   ### raw.coefficients <- drop(as.matrix(beta[-1])/sc$sigx)
   a00 <- if (intercept==FALSE) 0 else drop(sc$muy+beta[1]-as.vector(as.matrix(beta[-1]))%*%(sc$mux))
   raw.coefficients <- drop(as.matrix(beta[-1]))
   raw.residuals <- yy - cbind(1,xx) %*% c(a00,raw.coefficients)

   a00 <- a00+median(raw.residuals) ### PF: center again residuals
   raw.residuals <- yy - cbind(1,xx) %*% c(a00,raw.coefficients)

   raw.rmse <- sqrt(mean(raw.residuals^2))
   raw.wt <- weight.gaussian(raw.residuals,indexbest,del)$we ## weight vector

   if (missing(lambdaw)){
     ### search in a finer grid around minlambdaCrit:
#     loglambdamin <- log(fit$LambdaMin)
#     if (missing(lambdas)) {rl <- 0.5}
#     else {rl <- diff(range(lambdas))/10} ### PF
#     lseq <- seq(from=loglambdamin+rl,to=loglambdamin-rl,length=100)
#     lambdaw <- zeroSum(x[which(raw.wt==1),],y[which(raw.wt==1)],lambda=exp(lseq), ### new
#                        family,nFold=5,alpha=alphabest,
#                        standardize=FALSE,intercept=TRUE)$LambdaMin
     lambdaw <- zeroSum(x[which(raw.wt==1),],y[which(raw.wt==1)],
                        family,nFold=5,alpha=alphabest,
                        standardize=FALSE,intercept=FALSE)$LambdaMin
   }
   else if (!missing(lambdaw) & length(lambdaw)==1){
     lambdaw <- lambdaw
   }
   else if (!missing(lambdaw) & length(lambdaw)>1){
     lambdaw <- zeroSum(x[which(raw.wt==1),],y[which(raw.wt==1)],
                        family,lambda=lambdaw,nFold=5,alpha=alphabest,
                        standardize=FALSE,intercept=FALSE)$Lambdamin
   }
   fitw <- zeroSum(x[which(raw.wt==1),],y[which(raw.wt==1)],
                   family,alpha=alphabest, lambda=lambdaw,
                   standardize=FALSE,intercept=FALSE)
   ### betaw <- fitw$coef[[1]]
   betaw <- as.vector(coef(fitw))
   minlambdaCrit <- fitw$LambdaMin

   a0 <- if (intercept==FALSE) 0 else drop(sc$muy+betaw[1] -as.vector(as.matrix(betaw[-1]))%*%(sc$mux))
   ###coefficients <- drop(as.matrix(betaw[-1])/sc$sigx)
   coefficients <- drop(as.matrix(betaw[-1]))
   reweighted.residuals <-  yy - cbind(1,xx) %*% c(a0,coefficients)

   a0 <- a0+median(reweighted.residuals) ### PF: center again residuals
   reweighted.residuals <-  yy - cbind(1,xx) %*% c(a0,coefficients)

   reweighted.rmse <- sqrt(mean(reweighted.residuals^2))
   wgt <- weight.gaussian(reweighted.residuals,raw.wt==1,del)$we
   ## back transformed to the original scale


   num.nonzerocoef <- sum(coefficients!=0)

   intercept <- isTRUE(intercept)
   if(intercept) xx <- addIntercept(xx)

   if (intercept){
     coefficients <- c(a0,coefficients)
     raw.coefficients <- c(a00,raw.coefficients)
   } else {
     coefficients <- coefficients
        raw.coefficients <- raw.coefficients
   }


   raw.fitted.values <- xx %*% raw.coefficients
   fitted.values <- xx %*% coefficients


   objective <- h * ((1/2) * mean((yy[indexbest]-xx[indexbest,]%*%coefficients)^2) +   # is it correct to use indexbest? should not we use raw.wt?
                lambdabest * sum(1/2 * (1-alphabest) * coefficients^2 + alphabest*abs(coefficients)))


   if (intercept){
     coefficients <- coefficients[-1]
     raw.coefficients <- raw.coefficients[-1]
   }
   else {
     coefficients <- coefficients
     raw.coefficients <- raw.coefficients
   }

   inputs <- list(xx=xx,yy=yy,family=family,alphas=alphas,lambdas=lambdas,
                  hsize=hsize,h=h,nsamp=nsamp,s1=s1,nCsteps=nCsteps,nfold=nfold,
                  intercept=intercept,
                  repl=repl,para=para,ncores=ncores,del=del)

   output <- list(
      objective=objective,
      best=sort(indexbest),
      raw.wt=raw.wt,
      wt=wgt,
      a00=a00,
      raw.coefficients=raw.coefficients,
      a0=a0,
      coefficients=coefficients,
      alpha=alphabest,
      lambda=minlambdaCrit, ###lambdabest,
      lambdaw=lambdaw,
      num.nonzerocoef=num.nonzerocoef,
      h=h,
      raw.residuals=drop(raw.residuals),
      residuals=drop(reweighted.residuals),
      fitted.values=drop(fitted.values),
      raw.fitted.values=drop(raw.fitted.values),
      raw.rmse=raw.rmse,
      rmse=reweighted.rmse,
      inputs=inputs,
###      indexall=indexall,
###      beta=beta,
###      betaw=betaw,
      call=sys.call())


   class(output) <- "RobZS"

   output$call <- matchedCall
   output
}

####################################################################################################


