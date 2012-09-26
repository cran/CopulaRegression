# a is the model returned from the mle functions
# Rtest and Stest are the design matrices of the test data
# currently, there is no offset

# 

predict.copreg=function(object,Rtest,Stest,exposure=rep(1,nrow(Stest)),independence=FALSE,...){
	a=object
	x.pred<-as.vector(exp(Rtest%*%a$alpha))
	lambda<-as.vector(exp(Stest%*%a$beta))*exposure
	mu<-as.vector(exp(Rtest%*%a$alpha))
	y.pred<-lambda/(1-exp(-lambda))
	l.pred.ifm<-NULL
    if (independence==TRUE){
        l.pred<-x.pred*y.pred
	l.pred.ifm<-epolicy_loss(mu,a$delta,lambda,a$theta.ifm,a$family0)
    }
    else{
	l.pred<-epolicy_loss(mu,a$delta,lambda,a$theta,a$family)}
	return(list(x.pred=x.pred,y.pred=y.pred,l.pred=l.pred,l.pred.ifm=l.pred.ifm))


}
