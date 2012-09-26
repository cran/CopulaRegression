epolicy_loss <-
function(mu,delta,lambda,theta,family,y.max=300){
    if (length(mu)!= length(lambda)) stop("mu and lambda do not have the same length!")
    n<-length(mu)
    out<-vector(length=n)
    for (i in 1:n){
    #cat(paste("--- observation no ",i," ---\n"))
	foo<-function(l){
	dpolicy_loss(l,mu[i],delta,lambda[i],theta,family,y.max)*l
	}
	 out[i]<-integrate(foo,0,Inf)$value
    }
    return(out)
}


