#' cv.RobZS
#'
#' @param index
#' @param xx
#' @param yy
#' @param family
#' @param h
#' @param alphas
#' @param lambdas
#' @param nfold
#' @param repl
#' @param ncores
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
cv.RobZS <- function(index=NULL,xx,yy,family,h,alphas,
                        lambdas,nfold,repl,ncores,plot=TRUE){

   RTMSPE <- RMSPE <- NULL
   n <- nrow(xx); p <- ncol(xx)

   wh <- (alphas<0 | alphas>1)
   if (sum(wh)>0) stop("alphas can take the values only between 0 and 1")

   if (missing(alphas)) stop("provide an alphas sequence")
   if (missing(lambdas)) stop("provide an lambdas sequence")

   lambdaCrit <- evalCrit <- matrix(NA,nrow=length(lambdas),ncol=length(alphas))
   dimnames(lambdaCrit) <- dimnames(evalCrit) <- list(paste("lambdas",lambdas),paste("alpha",alphas))

   combis_ind <- expand.grid(1:length(lambdas), 1:length(alphas))
   indcombi <- 1:nrow(combis_ind)

   calc_evalCrit <- function(rowind, combis_ind, alphas, lambdas, index,
                             xx, yy,nfold, repl ){
      i <- combis_ind[rowind, 1]
      j <- combis_ind[rowind, 2]
      lambda <- lambdas[i]
      alpha <- alphas[j]

      print(paste("cross-validating for alpha: ", alpha, " and lambda :", lambda), sep = "")
      if(is.null(index)){
         x <- xx
         y <- yy
      } else {
         x <- xx[index[,i,j],]
         y <- yy[index[,i,j]]
      }
      evalCritl <- rep(NA,repl)

      for (l in 1:repl){ # repeated CV
            folds <- cvFolds(length(y), K = nfold, R = 1, type = "random")
            loss <- rep(NA,nrow(x))
         for (f in 1:nfold) {
               xtrain <- x[folds$subsets[folds$which != f,1], ]
               ytrain <- y[folds$subsets[folds$which != f,1] ]
               xtest <- x[folds$subsets[folds$which == f,1], ]
               ytest <- y[folds$subsets[folds$which == f,1] ]
            res <- tryCatch({
               hpen <- length(ytrain)
               trainmod <- zeroSum(xtrain,ytrain,family,alpha=alpha,lambda=lambda,
               											 standardize=FALSE, intercept=FALSE)},error=function(err){
                                     error <- TRUE
                                     return(error)})
            if (is.logical(res)){
               print(paste("CV broke off for alpha=",alpha ,"and lambda=", lambda))
            } else{
               trainmod <- res
               beta=trainmod$coef[[1]]
               loss[folds$which == f ] <- ytest - xtest %*% matrix(beta[-1])
            }
         }
            evalCritl[l] <- sqrt(mean(loss^2))
      }

      return(list(lambda_ind = i, alpha_ind = j, lambdatrue=res$LambdaMin,
                  evalCritl = evalCritl))
   }

   temp_result <- mclapply(1:nrow(combis_ind),
                           FUN = calc_evalCrit,
                           combis_ind = combis_ind,
                           alphas = alphas,
                           lambdas = lambdas,
                           index = index,
                           xx = xx,
                           yy = yy,
                           nfold = nfold,
                           repl = repl,
                           mc.cores = ncores,
                           mc.allow.recursive = FALSE
   )

   temp_result2 <- matrix(unlist(temp_result), ncol = repl + 3 , byrow = TRUE)
   for(k in 1:nrow(temp_result2)) {
      i <- temp_result2[k, 1]
      j <- temp_result2[k, 2]
      evalCrit[i,j] <- mean(temp_result2[k, 4:(repl+3)])
      lambdaCrit[i,j] <- temp_result2[k, 3]
   }

   optind <- which(evalCrit == min(evalCrit, na.rm = TRUE), arr.ind = TRUE)[1, ]
   minevalCrit <- evalCrit[optind[1],optind[2]]
   minlambdaCrit <- lambdaCrit[optind[1],optind[2]]
   indexbest <- index[,optind[1],optind[2]]
   alphas <- round(alphas,4)
   alpha <- alphas[optind[2]]
   lambdas <- round(lambdas,4)
   lambda <- lambdas[optind[1]]

   if (plot==TRUE){
      print(paste("optimal model: lambda =", lambda, "alpha =", alpha))
      lenCol <- length(alphas)*length(lambdas)
      mycol.b <- colorRampPalette(c("black","blue2", "purple", "orange", "yellow"))(lenCol)

      ggmspe <- evalCrit
      rownames(ggmspe) <- lambdas
      colnames(ggmspe) <- alphas
      ggmspe <- melt(ggmspe)
      if (is.null(index)){
            names(ggmspe) <- c("lambda","alpha","RTMSPE")
            mspeplot <- ggplot(ggmspe,aes(x=as.factor(lambda),y=as.factor(alpha),fill=RTMSPE)) +
               geom_tile() +  scale_fill_gradientn(colours=mycol.b) + theme(axis.text.x=element_text(angle=-90))
            mspeplot <- mspeplot +  ggtitle(paste0("RTMSPE (minimum at lambda=",lambda,",alpha=",alpha,",  ",family,")"))

      } else {
            names(ggmspe) <- c("lambda","alpha","RMSPE")
            mspeplot <- ggplot(ggmspe,aes(x=as.factor(lambda),y=as.factor(alpha),fill=RMSPE)) +
               geom_tile() +  scale_fill_gradientn(colours=mycol.b) + theme(axis.text.x=element_text(angle=-90))
            mspeplot <- mspeplot +  ggtitle(paste0("RMSPE (minimum at lambda=",lambda,",alpha=",alpha,",  ",family,")"))

      }
      mspeplot <- mspeplot + xlab("lambda") +  ylab("alpha")
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(1,1)))
      print(mspeplot, vp=viewport(layout.pos.row=1, layout.pos.col=1))
   }

   return(list(evalCrit=evalCrit,minevalCrit=minevalCrit,indexbest=indexbest,
   						lambdaopt=lambda,alphaopt=alpha,minlambdaCrit=minlambdaCrit))
}
