x=read.table("C:/Users/Akshay/Desktop/CaseStudy-2/spacedata.txt",header=FALSE)
y=read.table("C:/Users/Akshay/Desktop/CaseStudy-2/variousStatesDataset.txt",header=FALSE)

names(y)=c('Value','State')
#t=as.data.frame(prop.table(table(y[,1],y[,2]),1))

#ifelse(x==0,rownames(x),0)

#a=apply(x,2,table)
#l=do.call('c',lapply(a,function(x) x[[1]][[1]]))
#b=apply(x,1,table)
#rl=do.call('c',lapply(b,function(x) x[[1]][[1]]))
#summary(l)
#summary(rl)

#z=x[x!=0]
#hist(z)

#####Imputation of Missing Values
f=function(i,j){
  if(i==1 & j==1){
    v=c(temp[i+1,j],temp[i,j+1],temp[i+1,j+1])
  }
  if(i==1 & j==1000){
    v=c(temp[i+1,j],temp[i,j-1],temp[i+1,j-1])
  }
  if(i==1000 & j==1){
    v=c(temp[i-1,j],temp[i,j+1],temp[i-1,j+1])
  }
  if(i==1000 & j==1000){
    v=c(temp[i-1,j],temp[i,j-1],temp[i-1,j-1])
  }
  if(i==1 & j!=1 & j!=1000){
    v=c(temp[i+1,j],temp[i,j-1],temp[i+1,j+1],temp[i,j+1],temp[i+1,j-1])
  }
  if(i==1000 & j!=1 & j!=1000){
    v=c(temp[i-1,j],temp[i,j-1],temp[i-1,j+1],temp[i,j+1],temp[i-1,j-1])
  }
  if(j==1 & i!=1 & i!=1000){
    v=c(temp[i-1,j],temp[i,j+1],temp[i-1,j+1],temp[i+1,j+1],temp[i+1,j])
  }
  if(j==1000 & i!=1 & i!=1000){
    v=c(temp[i-1,j],temp[i,j-1],temp[i-1,j-1],temp[i+1,j-1],temp[i+1,j])
  }
  if(i>1 & i<1000 & j>1 & j<1000){
    v=c(temp[i-1,j],temp[i,j-1],temp[i-1,j-1],temp[i+1,j-1],temp[i+1,j],temp[i,j+1],temp[i-1,j+1],temp[i+1,j+1])   
  }
  round(median(do.call('c',lapply(v,function(xx)if(xx!=0) xx))))
}
  
temp=x
for(i in 1:1000)
{
  for(j in 1:1000)
  {
    temp[i,j]=ifelse(temp[i,j]==0,f(i,j),temp[i,j])
  }
}

#write.csv(temp,'C:/Users/Akshay/Desktop/CaseStudy-2/after_imputation.csv',row.names=F)

test=as.data.frame(as.vector(as.matrix(temp)))
names(test)='Value'
y$State=as.factor(y$State)

###Model Fitting for Prediction
library(e1071)
model=svm(State ~ Value, data=y, kernel="linear", scale=FALSE)
pred=predict(model,test)
df=data.frame(test,'State'=pred)

v2=as.numeric(as.character(df$State))
output=matrix(v2, ncol=1000, byrow=FALSE)

write.table(output,file='C:/Users/Akshay/Desktop/CaseStudy-2/output_file.txt',row.names=F,col.names=F)

#xxx=read.table("C:/Users/Akshay/Desktop/CaseStudy-2/output_file.txt",header=F)
