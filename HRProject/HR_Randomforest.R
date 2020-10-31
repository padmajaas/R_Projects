setwd("D:\\Padmaja\\EdvancerClass\\R\\HRProject")
getwd()

HR_train = read.csv("D:\\Padmaja\\EdvancerClass\\R\\HRProject\\hr_train.csv", stringsAsFactors = F)
HR_test = read.csv("D:\\Padmaja\\EdvancerClass\\R\\HRProject\\hr_test.csv", stringsAsFactors = F)

glimpse(HR_train)
glimpse(HR_test)


library(dplyr)
HR_test$left = NA
glimpse(HR_test)

HR_train$data="train"
HR_test$data = "test"
glimpse(HR_train)
glimpse(HR_test)

HR_full = rbind(HR_train,HR_test)
glimpse(HR_full)

summary(HR_full)



###----------------------------------------------------------------------------------------

### ------------------------Prroject 4 Part2---------------------------------------------------
###----------------------------------------------------------------------------------------


CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(HR_full)

sort(table(HR_full$sales))
sort(table(HR_full$salary))

HR_full = CreateDummies(HR_full,'sales',600)
glimpse(HR_full)

HR_full = CreateDummies(HR_full,'salary',1000)
glimpse(HR_full)

lapply(HR_full,function(x) sum(is.na(x)))


glimpse(HR_full)

HR_train = HR_full %>% filter(data == 'train') %>% dplyr :: select(-data)
HR_test = HR_full %>% filter(data == 'test') %>% dplyr :: select(-data,-left)

glimpse(HR_train)
glimpse(HR_test)

set.seed(62)
s=sample(1:nrow(HR_train),0.8*nrow(HR_train))
HR_train1=HR_train[s,]
HR_train2=HR_train[-s,]
glimpse(HR_train1)

library(car)





##--------------------------------------------------------------------------------------------
##----------------------------Random Forest Classification---------------------------------------
##--------------------------------------------------------------------------------------------


library(cvTools)
library(randomForest)
library(ggplot2)

## For classification tree we'll need to convert response to factor type

HR_train$left=as.factor(HR_train$left)
HR_train1$left = as.factor(HR_train1$left)
HR_train2$left = as.factor(HR_train2$left)

glimpse(HR_train)

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}

param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

num_trials=50
my_params=subset_paras(param,num_trials)
my_params


myauc=0
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  k=cvTuning(randomForest,left~.,
             data =HR_train,
             tuning =params,
             folds = cvFolds(nrow(HR_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    
    # uncomment the line above to keep track of progress
    best_params=params
  }
  print('DONE')
  # uncomment the line above to keep track of progress
}


myauc
best_params
##  0.8401224

HR.rf.final=randomForest(left~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=HR_train
)


HRtest.rf.score=predict(HR.rf.final,newdata = HR_test,type='prob')[,1]
write.table(HRtest.rf.score,'padmaja_Saripalli_P4_Part2_rf.csv',row.names = F)

## However if we needed to submit hard classes, we'll need to determine cutoff on the probability scores

train.score=predict(HR.rf.final,newdata=HR_train,type='prob')[,1]
View(train.score)
real=HR_train$left
View(real)

cutoffs=seq(0.001,0.999,0.001)
cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)
for(cutoff in cutoffs){
  predicted=as.numeric(train.score>cutoff)
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  P=TP+FN
  N=TN+FP
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  M=(4*FP+FN)/(5*(P+N))
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}
cutoff_data=cutoff_data[-1,]
View(cutoff_data)
my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
my_cutoff

##now that you have your cutoff, you can make hard predictions

HRtest.predicted=as.numeric(HRtest.rf.score>my_cutoff)
write.table(HRtest.predicted,"padmaja_Saripalli_P4_Part2_rf_hardclasses.csv",row.names = F)



##--------------------------------------------------------------------------------------
##-----------------------Gradient Boosting----------------------------------------------
##----------------------------------------------------------------------------------------

## For classification no need to convert to factor type for gbm
## we'll just change distribution to "bernoulli"
library(gbm)
library(cvTools)


## ------------------------------------------------------------------------
param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

num_trials=10
my_params=subset_paras(param,num_trials)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 10

## ----this code will take too long to run--------------------------------------------------------------
myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  # print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(gbm,left~.,
             data =HR_train,
             tuning =params,
             args=list(distribution="bernoulli"),
             folds = cvFolds(nrow(HR_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="response",n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}



## ------------------------------------------------------------------------
myauc

## ------------------------------------------------------------------------
best_params

## ------------------------------------------------------------------------
HR.gbm.final=gbm(left~.,data=HR_train,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsinnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "bernoulli")

## ----use these for prediciton and submission on test data--------------------------------------------------------------
test.score=predict(HR.gbm.final,newdata=HR_test,type='response',
                   n.trees = best_params$n.trees)
write.csv(test.score,"padmaja_Saripalli_P4_part2_gbm.csv",row.names=F)

