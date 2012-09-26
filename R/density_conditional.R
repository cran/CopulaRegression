density_conditional <-
function(y,x,mu,delta,lambda,theta,family){
    # define copula data
    x<-rep(x,length(y))
    u=pgam(x,mu,delta)
    v=pztp(y,lambda)
    vv<-pztp(y-1,lambda)
    # compute partial derivative
    out<-D_u(u,v,theta,family)- D_u(u,vv,theta,family)
    return(out)

}
