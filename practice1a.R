rm(list=ls())

###
#1. data import
###
url1="https://raw.githubusercontent.com/yunro-chung/2025_05_NAPPA/refs/heads/main/example1_wt_header.txt"
d1=read.table(file=url1,header=TRUE,sep=",")
d1

url2="https://raw.githubusercontent.com/yunro-chung/2025_05_NAPPA/refs/heads/main/example1_wo_header.txt"
d2=read.table(file=url2,header=TRUE,sep=",")
d2

url3="https://raw.githubusercontent.com/yunro-chung/2025_05_NAPPA/refs/heads/main/example1.csv"
d3=read.csv(file=url3,header=TRUE)
d3

#install.packages("openxlsx")
library(openxlsx)
url4="https://raw.githubusercontent.com/yunro-chung/2025_05_NAPPA/refs/heads/main/example1.xlsx"
sn="Sheet1"
d4=read.xlsx(xlsxFile=url4,sheet=sn)
#d4=openxlsx::read.xlsx(xlsxFile=url4,sheet=sn) same as the above

d4
colnames(d4)
dim(d4)
nrow(d4)
ncol(d4)

d4$Name
d4[,2]
d4[2,]
d4[1:3,1:3]
d4[c(1,2,3),c(1,2,3)]
d4[c(1,3),c(1,3)]
d4[,-2]

d5=as.matrix(d4)
d5
colnames(d5)
#d5$Namd #error
d5[,4]
as.numeric(d5[,4]) 

mean(d5[,4])
mean(as.numeric(d5[,4]))

###
#2. ROC
###
d=c(1,1,1,1,0,0,0,0,0)
y=c(1.41, 1.66, 1.99, 2.40,0.94, 1.11, 1.53, 1.66, 1.92)
ord_y=sort(unique(y))
m=length(ord_y)
th=c(-Inf,(ord_y[-1]+ord_y[-m])/2,Inf)

n=length(th)
y1=y[which(d==1)]
y0=y[which(d==0)]

st=sp=rep(NA,n)
for(i in 1:n){
  st[i]=mean(y1>th[i])
  sp[i]=mean(y0<=th[i])
}
cbind(th,st,sp)

tpf=st
fpf=1-sp
plot(tpf~fpf, ylab="Sensitivity",xlab="1-Specificity")
plot(tpf~fpf, ylab="Sensitivity",xlab="1-Specificity", type='l')


#install.packages('pROC')
library(pROC)
fit=roc(d~y)
plot(fit)
cbind(th=fit$thresholds,st=fit$sensitivities,sp=fit$specificities)
