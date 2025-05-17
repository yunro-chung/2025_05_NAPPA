rm(list=ls())

#1. Import data
url="https://raw.githubusercontent.com/yunro-chung/2025_05_NAPPA/refs/heads/main/dat0.csv"
dat0=read.csv(url,header=FALSE)

#2. separate proteins
dat1=dat0[-grep("REG",dat0[,3]),]            #proteins of interests
dat_pc=dat0[dat0[,4]%in%c("EBNA","hIgG"),]   #positive control spot
dat_nc=dat0[dat0[,4]%in%c("MM","non-spot"),] #nonspot control spot

#3. id, outcome & protein
id=as.character(dat1[1,-c(1:4)]) #sample id

y=as.character(dat1[2,-c(1:4)])  #outcome
y[y=="BLBC"]=1
y[y=="Control"]=0
y=as.numeric(y)
  
pname=dat1[-c(1:3),4]   #protein names

#4. protein
x=dat1[-c(1:3),-c(1:4)]      #protein
x_pc=dat_pc[-c(1:3),-c(1:4)] #positive control
x_nc=dat_nc[-c(1:3),-c(1:4)] #nonspot control

#5. normalization
method="median" #(or method="quantile")
log2tr="yes"    #log2 transformation

p=nrow(x)
n=ncol(x)
for(i in 1:n)
  x[,i]=as.numeric(x[,i])

x1=matrix(NA,p,n)   #initialization for normalized data
for(i in 1:n)
  x1[,i]=as.numeric(x[,i])

if(method=="quantile"){
  med=median(x1)
  for(i in 1:n){ #ith sample
    bc=quantile(as.numeric(x_nc[,i]),p=0.25)
    x1[,i]=(x1[,i]-bc)/(med-bc)
  }
}else if(method=="median"){
  for(i in 1:n){ #ith sample
    med_p=median(as.numeric(x_pc[,i]))
    med_n=median(as.numeric(x_nc[,i]))
    x1[,i]=x1[,i]/(med_p-med_n)
  }
}

if(log2tr=="yes")
  x1=log2(x1)

#6. summary
x2=t(x1) #transpose for wide-format
dat2=data.frame(id=id,y=y,x=x2)
rownames(dat2)=NULL
colnames(dat2)=c("id","y",pname)

#write.csv(dat2,'dat2.csv',row.names=FALSE)