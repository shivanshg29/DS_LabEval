#Question 1
sub1<-sample(50:100,20,replace = FALSE)
sub2<-sample(50:100,20,replace = FALSE)
sub3<-sample(50:100,20,replace = FALSE)

marks<-cbind(sub1,sub2,sub3)
marks

total<-apply(marks,1,sum)
marks<-cbind(marks,total)
marks

sd.err<-function(x){
  sd(x)/sqrt(length(x))
}

apply(marks,2,sd.err)

apply(marks[,1:3],2,function(x) x+0.25)

#Question 2-6
v1<-sub1
v2<-sub2
v3<-sub3
lapply(list(v1,v2,v3),sum)
sapply(list(v1,v2,v3),sum)
sapply(cbind(v1,v2,v3),sqrt)

I =c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
marks<-data.frame(cbind(I,marks))
marks
tapply(marks$sub1,marks$I,mean)
tapply(marks$sub1,marks$I,sd)

f<-function(x,y){
  x/y
}
mapply(f,v1,v2)

#Question 7
data("Seatbelts")
Seatbelts
