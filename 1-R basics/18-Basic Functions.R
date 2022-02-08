
avg<- function(x){
  s<- sum(x)
  n<- length(x)
  avg<-s/n
}

x<-1:100
y<-avg(x)
y

identical(mean(x), avg(x))

s<-3
u<-avg(1:10)
u
#The new definition for average - arithmetic or geometric 
avg<- function(x, arithmetic=TRUE){
  n<- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
avg(1:10, FALSE)
avg(1:10, TRUE)
