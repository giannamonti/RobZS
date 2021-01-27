
selectbest10zero <- function(x,y,family,h,hsize,alpha,lambda,nsamp,s1,para,ncores,seed) {
   obj <- NULL
   all_subsets <- InitialSubset0(x,y,family,h,hsize,alpha,lambda,nsamp,para,ncores, seed)
   subsets <- all_subsets$subsets
   index.subsets <- all_subsets$index.subsets
   if (para){
      obj <- unlist(mclapply(1:nsamp, function(ob,sub){
         ob_val <- subsets[[ob]]$obj
      }, subsets, mc.cores = ncores, mc.allow.recursive = FALSE))
   }else{
      for (i in 1:nsamp){ obj <- c(obj,subsets[[i]]$obj) }
   }

   if(family=="binomial"){
      obj_sorted <- sort(obj,decreasing=TRUE,index.return=TRUE)
   } else if(family=="gaussian"){
      obj_sorted <- sort(obj,decreasing=FALSE,index.return=TRUE)
   }

   obj <- obj_sorted$x[1:s1]
   s1_new <- length(obj[!is.infinite(obj)])
   idx <- obj_sorted$ix[1:s1_new]
   if (s1_new==0){
      stop(paste("Model is not suitable for alpha",alpha,"lambda",
                 lambda,"for this data set. Choose another lambda."))
   }
   if (para){
      bestindex <- mclapply(1:s1_new, function(c,idx,subsets) {
         indx <- subsets[[idx[c]]]$indx
      },idx,subsets,mc.cores = ncores)
   }else{
      bestindex <- lapply(1:s1_new, function(c,idx,subsets) {
         indx <- subsets[[idx[c]]]$indx
      },idx,subsets)
   }
   return(list(idxbest=bestindex,s1_new=s1_new,subsets=subsets,index.subsets=index.subsets))
}


InitialSubset0 <- function(x,y,family,h,hsize,alpha,lambda,nsamp,para,ncores,seed) {
   # gives initial 500 subsamples after Two C Steps
   if (!is.null(seed)) set.seed(1)

   if (family=="binomial"){
      index.subsets <- replicate(nsamp,c(sample(which(y==1),2),sample(which(y==0),2)))
   } else if (family=="gaussian"){
      index.subsets <- replicate(nsamp,sample.int(nrow(x), 3))
   }

   twoCstep <- function(c,x,y,family,index.subsets,h,hsize,alpha,lambda){

      ## C step 1
      if(floor(c/100)==c/100) print(c)

      Cstep1 <- CStep0(x,y,family,index.subsets[,c],h,hsize,alpha,lambda)
      indx1 <- Cstep1$index
      object1 <- Cstep1$object

      ## C step 2
      Cstep2 <- CStep0(x,y,family,indx1,h,hsize,alpha,lambda) # h observations
      indx2 <- Cstep2$index
      object <- Cstep2$object
      return(list(obj=object,indx=indx2))
   }
   if (para){
      subsets <- mclapply(1:nsamp,
                          FUN = twoCstep,
                          x = x, y = y,
                          family = family,
                          index.subsets=index.subsets,
                          h = h, hsize = hsize,
                          alpha = alpha,
                          lambda = lambda,
                          mc.cores = ncores)
   } else {
      subsets <- lapply(1:nsamp,
                        FUN = twoCstep,
                        x = x, y = y,
                        family = family,
                        index.subsets=index.subsets,
                        h = h, hsize = hsize,
                        alpha = alpha,
                        lambda = lambda)
   }
   return(list(subsets=subsets,index.subsets=index.subsets))
}

