rm(list=ls())

#1. Import data
url2="https://raw.githubusercontent.com/yunro-chung/2025_05_NAPPA/refs/heads/main/dat2.csv"
dat2=read.csv(url2)

id=dat2$id
y=dat2$y
x=as.matrix(dat2[,-c(1:2)])
pname=colnames(x)

#2. summary statistics
eda=function(x){
  avg=apply(x,MARGIN=2,FUN=mean)
  sd=apply(x,MARGIN=2,FUN=sd)
  med=apply(x,MARGIN=2,FUN=median)
  q1=apply(x,MARGIN=2,FUN=quantile,prob=c(0.25))
  q3=apply(x,MARGIN=2,FUN=quantile,prob=c(0.75))
  iqr=q3-q1
  
  return(data.frame(avg=avg,sd=sd,med=med,q1=q1,q3=q3,iqr=iqr))
}
x1=x[which(y==1),] #submatrix consisting of y=1
x0=x[which(y==0),] #submatrix consisting of y=0
eda1=eda(x1)
eda0=eda(x0)

round(eda1,2)
round(eda0,2)

k=which.max(eda1$avg/eda0$avg) 

#2. box plot
xk=x[,k]
boxplot(xk~y,ylab='Normalized intensity',xlab="Group",names=c('Control','BLBC'))

#3. histogram & qq plot
xk1=xk[which(y==1)]
xk0=xk[which(y==0)]

par(mfrow=c(2,2))
hist(xk1,freq=FALSE,xlab='Normalized intensity',main='Histogram (BLBC)')
qqnorm(xk1,pch=19,main=''); qqline(xk1)

hist(xk0,freq=FALSE,xlab='Normalized intensity',main='Histogram (Control)')
qqnorm(xk0,pch=19,main=''); qqline(xk0)
par(mfrow=c(1,1))

#4. Shapiro-Wilk test
shapiro.test(xk1) #p>0.05 fail to reject the H0: data follow a normal distribution.
shapiro.test(xk0)

#5. t.test / Wilcoxon rank-sum test (type I error inflation)
t.test(xk~y)
wilcox.test(xk~y)

#7. adjusted p-values
alpha=0.05
p=ncol(x)
pval=rep(NA,p)
for(j in 1:p)
  pval[j]=t.test(x[,j]~y)$p.value

#bonferroni
alpha=0.05
adj_pval1=p.adjust(pval,method='bonferroni')
sum(adj_pval1<alpha)

#Benjamini and Hochberg correction
adj_pval2=p.adjust(pval,method='BH')
sum(adj_pval2<alpha)

#7. ROC curve
library(pROC)
q=which.min(adj_pval2)
xq=x[,q]
fit=roc(y,xq,levels=c(0,1),direction="<")
plot(fit)

