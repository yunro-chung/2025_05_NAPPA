rm(list=ls())

#1. Import data
url2="https://raw.githubusercontent.com/yunro-chung/2025_05_NAPPA/refs/heads/main/dat2.csv"
dat2=read.csv(url2)

id=dat2$id
y=dat2$y
x=as.matrix(dat2[,-c(1:2)])
pname=colnames(x)
n=nrow(x)
p=ncol(x)

library(pROC)

#2. lasso
library(glmnet)
alpha=1 #lasso
#alpha=0.5 #elastic-net
#alpha=0 #ridge

#fitted
fit_tr=try(cv.glmnet(x=x, y=y, family="binomial", alpha=alpha),silent=TRUE) # lasso.fit
#p_tr=as.numeric(predict(fit_tr, newx=x, type="response"))  (overfitted)
#roc_tr=roc(y,p_tr,levels=c(0,1),direction="<")
#plot(roc_tr)
#roc_tr$auc
coef(fit_tr, s="lambda.min")
coef(fit_tr, s="lambda.min")[which(abs(coef(fit_tr, s="lambda.min"))>0),]

#loocv
p_te=rep(NA,n)
for(i in 1:n){
  print(i)
  y_tr=y[-i]
  x_tr=x[-i,]
  
  #y_te=y[i]
  x_te=x[i,]
  
  fit_cv=cv.glmnet(x=x_tr, y=y_tr, family="binomial", alpha=alpha)
  p_te[i]=predict(fit_cv, newx=x_te, s="lambda.min", type="response")
}
roc_te=roc(y,p_te,levels=c(0,1),direction="<")
plot(roc_te)
roc_te$auc

#3. radnom forest
library(randomForest)

#fitted
y_fac=as.factor(y)
fit_tr=randomForest(x=x,y=y_fac,importance=TRUE)
importance(fit_tr,type=2) #1=mean decrease in accuracy, 2=mean decrease in node impurity

imp=importance(fit_tr,type=2)
imp=data.frame(pname=rownames(imp),MeanDecreaseGini=imp)
rownames(imp)=NULL
imp=imp[order(imp$MeanDecreaseGini,decreasing=TRUE),]
imp[1:10,]

#loocv
p_te=rep(NA,n)
for(i in 1:n){
  print(i)
  y_fac_tr=y_fac[-i]
  x_tr=x[-i,]
  
  x_te=x[i,]
  
  fit_cv=randomForest(x=x_tr,y=y_fac_tr)
  pred=predict(fit_cv, newdata=x_te, type="prob")
  p_te[i]=pred[which(colnames(pred)=="1")]
}
roc_te=roc(y,p_te,levels=c(0,1),direction="<")
plot(roc_te)
roc_te$auc

#4. MRMR+logistic
library(mRMRe)
FCOUNT=5 #number of selected markers

#fitted
mrmr_data=mRMR.data(data=data.frame(cbind(y,x)))
filter_tr=mRMR.classic(data=mrmr_data, target_indices=1, feature_count=FCOUNT) 
mrmr_mark=as.numeric(solutions(filter_tr)$'1')-1 #-1 for target variable

x_sel=x[,mrmr_mark]
fit_tr=glm(y~x_sel, family = binomial(link=logit))
fit_tr

#loocv
p_te=rep(NA,n)
for(i in 1:n){
  print(i)
  
  y_tr=y[-i]
  x_tr=x[-i,]
  
  mrmr_data=mRMR.data(data=data.frame(cbind(y_tr,x_tr)))
  filter_tr=mRMR.classic(data=mrmr_data, target_indices=1, feature_count=FCOUNT) 
  mrmr_mark=as.numeric(solutions(filter_tr)$'1')-1 #-1 for target variable
  
  x_tr_sel=x_tr[,mrmr_mark]
  x_te_sel=x[i,mrmr_mark]
  
  fit_cv=glm(y_tr~x_tr_sel, family = binomial(link=logit))
  p_te[i]=1/(1+exp(-sum(coef(fit_cv)*c(1,x_te_sel))))
}

roc_te=roc(y,p_te,levels=c(0,1),direction="<")
plot(roc_te)
roc_te$auc

#5. MRMR+rf
FCOUNT=5

#fitted
mrmr_data=mRMR.data(data=data.frame(cbind(y,x)))
filter_tr=mRMR.classic(data=mrmr_data, target_indices=1, feature_count=FCOUNT) 
mrmr_mark=as.numeric(solutions(filter_tr)$'1')-1 #-1 for target variable

x_sel=x[,mrmr_mark]
fit_tr=randomForest(x=x_sel,y=y_fac)
importance(fit_tr,type=2)

#loocv
p_te=rep(NA,n)
for(i in 1:n){
  print(i)
  
  y_fac_tr=y_fac[-i]
  y_tr=y[-i]
  x_tr=x[-i,]
  
  mrmr_data=mRMR.data(data=data.frame(cbind(y_tr,x_tr)))
  filter_tr=mRMR.classic(data=mrmr_data, target_indices=1, feature_count=FCOUNT) 
  mrmr_mark=as.numeric(solutions(filter_tr)$'1')-1 #-1 for target variable
  
  x_tr_sel=x_tr[,mrmr_mark]
  x_te_sel=x[i,mrmr_mark]
  
  fit_cv=randomForest(x=x_tr_sel,y=y_fac_tr)
  pred=predict(fit_cv, newdata=x_te_sel, type="prob")
  p_te[i]=pred[which(colnames(pred)=="1")]
}
roc_te=roc(y,p_te,levels=c(0,1),direction="<")
plot(roc_te)
roc_te$auc

#6. MRMR+logistic vs MRMR+rf
FCOUNT=3:12
M=length(FCOUNT)
fit_tr1=fit_tr2=roc_te1=roc_te2=list()
auc1=auc2=pauc1=pauc2=st1=st2=rep(NA,M)
fix=0.8
for(m in 1:M){
  #fitted
  mrmr_data=mRMR.data(data=data.frame(cbind(y,x)))
  filter_tr=mRMR.classic(data=mrmr_data, target_indices=1, feature_count=FCOUNT[m]) 
  mrmr_mark=as.numeric(solutions(filter_tr)$'1')-1 #-1 for target variable
  
  x_sel=x[,mrmr_mark]
  fit_tr1[[m]]=glm(y~x_sel, family = binomial(link=logit))
  fit_tr2[[m]]=randomForest(x=x_sel,y=y_fac)

  #loocv
  p_te1=p_te2=rep(NA,n)
  for(i in 1:n){
    print(c(m,i))
    
    y_fac_tr=y_fac[-i]
    y_tr=y[-i]
    x_tr=x[-i,]
    
    mrmr_data=mRMR.data(data=data.frame(cbind(y_tr,x_tr)))
    filter_tr=mRMR.classic(data=mrmr_data, target_indices=1, feature_count=FCOUNT[m]) 
    mrmr_mark=as.numeric(solutions(filter_tr)$'1')-1 #-1 for target variable
    
    x_tr_sel=x_tr[,mrmr_mark]
    x_te_sel=x[i,mrmr_mark]
    
    fit_cv1=glm(y_tr~x_tr_sel, family = binomial(link=logit))
    p_te1[i]=1/(1+exp(-sum(coef(fit_cv1)*c(1,x_te_sel))))
    
    fit_cv2=randomForest(x=x_tr_sel,y=y_fac_tr)
    pred=predict(fit_cv2, newdata=x_te_sel, type="prob")
    p_te2[i]=pred[which(colnames(pred)=="1")]
  }
  
  roc_te1[[m]]=roc(y,p_te1,levels=c(0,1),direction="<")
  roc_te2[[m]]=roc(y,p_te2,levels=c(0,1),direction="<")
  
  auc1[m]=roc_te1[[m]]$auc
  auc2[m]=roc_te2[[m]]$auc
  
  st1[m]=coords(roc_te1[[m]],x=fix,input='specificity',ret="sensitivity")$sensitivity
  st2[m]=coords(roc_te2[[m]],x=fix,input='specificity',ret="sensitivity")$sensitivity
  
  pauc1[m]=roc(y,p_te1,levels=c(0,1),direction="<",partial.auc=c(1,fix),partial.auc.focus='specificity')$auc
  pauc2[m]=roc(y,p_te2,levels=c(0,1),direction="<",partial.auc=c(1,fix),partial.auc.focus='specificity')$auc
}

#auc
ylim=range(c(auc1,auc2))
plot(auc1~FCOUNT,ylab="AUC",xlab="Number of markers",ylim=ylim,pch=19)
points(auc2~FCOUNT,pch=19,col=2)
legend('topright',c("Logistic","RF"),col=c(1,2),pch=c(19,19))
k1=which.max(auc1)
FCOUNT[k1]
auc1[k1]
fit_tr1[[k1]]

k2=which.max(auc2)
FCOUNT[k2]
auc1[k2]
importance(fit_tr2[[k2]],type=2)

plot(y=roc_te1[[k1]]$sensitivities,x=1-roc_te1[[k1]]$specificities,type='l', ylab="Sensitivity",xlab="1-Specificity")
lines(y=roc_te2[[k1]]$sensitivities,x=1-roc_te2[[k1]]$specificities,type='l',col=2)
legend('bottomright',c("Logistic","RF"),col=c(1,2),lty=c(1,1))

#st at 90% sp
ylim=range(c(st1,st2))
plot(st1~FCOUNT,ylab="St at 90% sp",xlab="Number of markers",ylim=ylim,pch=19)
points(st2~FCOUNT,pch=19,col=2)
legend('topright',c("Logistic","RF"),col=c(1,2),pch=c(19,19))
k1=which.max(st1)
FCOUNT[k1]
st1[k1]
fit_tr1[[k1]]

k2=which.max(st2)
FCOUNT[k2]
st2[k2]
importance(fit_tr2[[k2]],type=2)

plot(y=roc_te1[[k1]]$sensitivities,x=1-roc_te1[[k1]]$specificities,type='l', ylab="Sensitivity",xlab="1-Specificity")
lines(y=roc_te2[[k1]]$sensitivities,x=1-roc_te2[[k1]]$specificities,type='l',col=2)
legend('bottomright',c("Logistic","RF"),col=c(1,2),lty=c(1,1))

#PAUC
ylim=range(c(st1,st2))
plot(st1~FCOUNT,ylab="PAUC between 90-100% sp",xlab="Number of markers",ylim=ylim,pch=19)
points(st2~FCOUNT,ylim=ylim,pch=19,col=2)
legend('topright',c("Logistic","RF"),col=c(1,2),pch=c(19,19))
k1=which.max(st1)
FCOUNT[k1]
pauc1[k1]
fit_tr1[[k1]]

k2=which.max(st2)
FCOUNT[k2]
pauc1[k2]
importance(fit_tr2[[k2]],type=2)

plot(y=roc_te1[[k1]]$sensitivities,x=1-roc_te1[[k1]]$specificities,type='l', ylab="Sensitivity",xlab="1-Specificity")
lines(y=roc_te2[[k1]]$sensitivities,x=1-roc_te2[[k1]]$specificities,type='l',col=2)
legend('bottomright',c("Logistic","RF"),col=c(1,2),lty=c(1,1))