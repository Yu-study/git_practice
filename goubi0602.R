rm(list = ls())
##used for goubi's lib data
##calculate integral value of some scatter point
#notice: calculate level detial 1:n+1 we calculate 1:n then +1
library(stringr)
library(ggplot2)
library(pracma)
##define a integral function
reccu<-function(x1,x2,y1,y2){
  mx<-as.matrix(x1,x2,y1,y2)
  value<-1/2*(x2-x1)*(y1+y2)
  return(value)
}
area<-function(df,xnum=1,ynum=2){
  x=df[,xnum]
  index=order(x)
  df=df[index,]
  df=as.matrix(df)
  areavalue=0
  for(i in 1:(nrow(df)-1)){
    areavalue=areavalue+reccu(df[i,xnum],df[i+1,xnum],df[i,ynum],df[i+1,ynum])
  }
  return(areavalue)
}
inputpath<-"C:/Users/Elic/Desktop/goubi/"
goalfiles<-dir(inputpath)
goalfiles<-goalfiles[which(goalfiles!="6p.zip")]
# for(cc in goalfiles){
#   temp1<-read.table(str_c(inputpath,cc),skip = 4)
#   names(temp1)<-c("x","y")
#   qplot(x,y,data = temp1)
# }
dd1<-read.table(str_c(inputpath,"00-0.txt"),skip = 4)
names(dd1)<-c("x","y")
qplot(x,y,data = dd1)
dd1_1=dd1[1:which.max(dd1$x),]
dd1_2=dd1[which.max(dd1$x):nrow(dd1),]
integration<-area(dd1_1,1,2)-area(dd1_2,1,2)
integration
