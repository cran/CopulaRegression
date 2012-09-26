vuongtest<-function(model1,model2){
    ll1<-model1$ll
    ll2<-model2$ll
    m<-ll1-ll2
    t.stat<-sqrt(length(m))*mean(m)/sd(m)
    return(t.stat)



}
