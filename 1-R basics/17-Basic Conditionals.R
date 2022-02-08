
a<-8
if (a!=0){
  print(1/a)
}else{
  print("No reciprocal for 0.")
}

library(dslabs)
data(murders)
murder_rate<-murders$total/murders$population*100000
ind<- which.min(murder_rate)
if(murder_rate[ind]<0.5){
  print(murders$state[ind])
}else{
  print("No state has murder rate that low")
}


ind<- which.min(murder_rate)
if(murder_rate[ind]<0.25){
  print(murders$state[ind])
}else{
  print("No state has murder rate that low")
}

a<-0
ifelse(a>0, 1/a, NA)

a<-c(0,1,2,-4,5)
result<-ifelse(a>0, 1/a, NA)
result

data("na_example")
sum(is.na(na_example))
no_nas<-ifelse(is.na(na_example),0,na_example)
no_nas
sum(is.na(no_nas))

z<- c(TRUE,TRUE,FALSE)
any(z)
all(z)

z<-c(FALSE,FALSE,FALSE)
any(z)
all(z)

z<- c(TRUE,TRUE,TRUE)
any(z)
all(z)
