get_qda<-function(x,y,x0, verbose=FALSE){
# get data
N<-length(x)
M<-length(x0)
K<-length(unique(y))
Q<- sort(unique(y))
df<-tibble(x,y)
# filter
df<-df|>drop_na()                           # remove missing values
stopifnot(length(df$x)==length(df$y),       # filter missing values            
          length(unique(df$y))>=2,          # filter <2 classes
          c(table(df$y))>=2)                # filter <2 classes / class
# calc mean & prob by class
df<-df%>%
  group_by(y)%>%
  mutate(
    mu=mean(x),
    pi=n() / N
  )
# calc var by class
df<-df%>%
  group_by(y)%>%
  mutate(
    s2=sum((x-mu)^2) / (n()-1)
  )
# select class statistics 
df2<-df%>%slice(1)
s2=df2$s2
stopifnot(sapply(s2, function(x) any(x>0)))
mu = df2$mu
pi=df2$pi
#calculate delta
delta<-matrix(, nrow = M, ncol = K)
for (j in 1:M){
  for (i in 1:K){
    delta[j ,i]<- -0.5*x0[j]^2/s2[i]+x0[j]*mu[i]/s2[i]-0.5*mu[i]^2/s2[i]-0.5*log(abs(s2[i]))+log(pi[i])
  }
}
# match delta to class
delta<-t(delta)
delta<-as.data.frame(abs(delta))
delta<-cbind(delta, Q)
qda<-c()
for (i in 1:M){
  qda[i]<-delta$Q[which.min(delta[ ,i])]
}
return(qda)
}