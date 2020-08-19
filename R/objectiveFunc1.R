
Objval0 <- function(x,y,family,coef,ind=NULL,alpha,lambda){
   # Value of objective function:
   if (is.null(ind)){
         obj <- (1/2) * mean((y - x %*% coef)^2) + lambda * sum(1/2 * (1-alpha) * coef^2 + alpha*abs(coef))
      } else {
         obj <- (1/2) * mean((y[ind] - x[ind,] %*% coef)^2) + lambda * sum(1/2 * (1-alpha) * coef^2 + alpha*abs(coef))
         }
   return(obj)
}
