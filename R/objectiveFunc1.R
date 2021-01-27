
Objval0 <- function(x,y,family,coef,ind=NULL,alpha,lambda){
	# Value of objective function:

	if (is.null(ind)){
		if (family=="binomial"){
			obj <- mean(phiBY3(x%*%coef,ifelse(y==0,1,0),c3=0.5))
		}else if(family=="gaussian"){
			obj <- (1/2) * mean((y - x %*% coef)^2) + lambda * sum(1/2 * (1-alpha) * coef^2 + alpha*abs(coef))
		}
	} else{
		if (family=="binomial"){
			obj <- mean(phiBY3(x[ind,]%*%coef,ifelse(y[ind]==0,1,0),c3=0.5))
		} else if(family=="gaussian"){
			obj <- (1/2) * mean((y[ind] - x[ind,] %*% coef)^2) + lambda * sum(1/2 * (1-alpha) * coef^2 + alpha*abs(coef))
		}
	}
	return(obj)
}
