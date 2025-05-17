rm(list=ls())

#1. Import data
url2="https://raw.githubusercontent.com/yunro-chung/2025_05_NAPPA/refs/heads/main/dat2.csv"
dat2=read.csv(url2)

id=dat2$id
y=dat2$y
x=as.matrix(dat2[,-c(1:2)])
pname=colnames(x)

library(pROC)

#2. plot ROC
j=1 #1st marker
xj=x[,j]
fit1=roc(y,xj,levels=c(0,1),direction="<")
plot(fit1)

#3. auc
auc(fit1)
ci(fit1$auc)

#4. st at a fixed sp / sp at a fixed sp
fix=0.9
coords(fit1,x=fix,input='specificity',ret="sensitivity")
ci.coords(fit1,x=fix,input='specificity',ret="sensitivity")

coords(fit1,x=fix,input='sensitivity',ret="specificity")
ci.coords(fit1,x=fix,input='sensitivity',ret="specificity")

#5. pauc between (1,fix) sp / (1,fix) st
fit2=roc(y,xj,levels=c(0,1),direction="<",partial.auc=c(1,fix),partial.auc.focus='specificity')
fit2$auc
ci(fit2)

fit3=roc(y,xj,levels=c(0,1),direction="<",partial.auc=c(1,fix),partial.auc.focus='sensitivity')
fit3$auc
ci(fit3)

#6. choose the marker with the highest auc
p=ncol(x)
auc=rep(NA,p)
for(j in 1:p)
  auc[j]=roc(y,x[,j],levels=c(0,1),direction="<")$auc

k=which.max(auc)
pname[k]
auc[k]
fit=roc(y,x[,k],levels=c(0,1),direction="<")
plot(fit)
fit$auc
