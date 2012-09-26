ppolicy_loss <-
function(l,mu,delta,lambda,theta,family,y.max=20){
    out<-vector(length=length(l))
    if (length(mu)==1) mu=rep(mu,length(l))
    if (length(lambda)==1) lambda=rep(lambda,length(l))
    for (i in 1:length(l)){
    foo<-function(s){
	dpolicy_loss(s,mu[i],delta,lambda[i],theta,family,y.max)
	}
	 out[i]<-integrate(foo,0,l[i])$value
    }
    return(out)
}


