setwd("D:\\Padmaja\\EdvancerClass\\R\\BankingProject")
getwd()

bank_full_train = read.csv("D:\\Padmaja\\EdvancerClass\\R\\BankingProject\\bank_full_train.csv", stringsAsFactors = F)
bank_full_test = read.csv("D:\\Padmaja\\EdvancerClass\\R\\BankingProject\\bank_full_test.csv", stringsAsFactors = F)

glimpse(bank_full_train)
glimpse(bank_full_test)

library(dplyr)


bank_full_test$y = NA
glimpse(bank_full_test)

bank_full_train$data = "train"
bank_full_test$data = "test"
glimpse(bank_full_train)
glimpse(bank_full_test)

bank_full = rbind(bank_full_train,bank_full_test)

glimpse(bank_full)


###-------------------------------------------------------------------------

##  Project 5 Part 1

##1 Find mean of the variable age. Round off to 2 decimal places.


mean(bank_full$age)

round(mean(bank_full$age,na.rm = T),2)

## 2. Total number of outliers present in the variable balance.
##Use 'Q1-1.5*IQR' to calculate lower limit and 'Q3 + 1.5×IQR' to calculate upper limit.
##calculate the count of values in variable balance which are beyond these limits.
outlier_upper=function(x){
  
  q3 = quantile(x, 0.75,na.rm = T) 
  
  iqr = IQR(x, na.rm=T)
  return(q3+1.5*iqr);
}

outlier_lower = function(x){
  q1 = quantile(x, 0.25,na.rm = T)
  iqr = IQR(x,na.rm = T)
  return(q1-1.5*iqr);
}
glimpse(bank_full)
calcoutlierDat = bank_full[,c("balance","duration")]

glimpse(calcoutlierDat)

## returns the upper and lower outlier values for each column


apply(calcoutlierDat,2,outlier_upper)
apply(calcoutlierDat,2,outlier_lower)

## Calculate the outliers for all the numeric columns

apply(calcoutlierDat,2,function(x) sum(x>outlier_upper(x)))
apply(calcoutlierDat,2,function(x) sum(x<outlier_lower(x)))

##  4729

## 3. Find the variance of variable balance.

var(bank_full$balance,na.rm = T)

## 4. which function is used to remove multicollinearity among variables
##  vif

## 7. Does validation help in generalising the model?
## yes

##  8.Whether the data given (train data) is a balanced or extremely imbalanced data
##( ratio of response class counts even more extreme than 5%:95%)? 
## Note: you need to write either 'Balanced' or 'Imbalanced' . 
## Any further details will result in your answer being marked as wrong. 
## Answers are not case sensitive

table(bank_full_train$y)
count(bank_full_train)
count(bank_full_test)

## 9. How is box plot upper whisker is calculated ? Choose out of these:


##------------------------------------------------------------------------------------
##------------------------End Part1 ------------------------------------------------------
##----------------------------------------------------------------------------------------


### -----------------------------Project Part2 ----------------------------------------




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

View(bank_full)


sort(table(bank_full$job))

bank_full = CreateDummies(bank_full,"job",1000)
glimpse(bank_full)


sort(table(bank_full$marital))
bank_full = CreateDummies(bank_full,"marital",5000)
glimpse(bank_full)

sort(table(bank_full$default))
bank_full$default=as.numeric(bank_full$default=="yes")
glimpse(bank_full)

sort(table(bank_full$housing))
bank_full$housing=as.numeric(bank_full$housing=="yes")
glimpse(bank_full)


sort(table(bank_full$loan))
bank_full$loan=as.numeric(bank_full$loan=="yes")
glimpse(bank_full)


## round(prop.table((table(bank_full$pdays,bank_full$y)),1),1)

#bank_full$pdays[bank_full$pdays==-1] <- 0
glimpse(bank_full)


sort(table(bank_full$poutcome))
bank_full = CreateDummies(bank_full,"poutcome",1500)
glimpse(bank_full)

bank_full=bank_full %>%
  dplyr::select(-age,-education,-contact,-day,-month,-ID)

glimpse(bank_full)

lapply(bank_full,function(x) sum(is.na(x)))

bank_train = bank_full %>% filter(data=='train') %>% dplyr::select(-data)
bank_test = bank_full %>% filter(data=='test') %>% dplyr::select (-data,-y)
glimpse(bank_train)
glimpse(bank_test)

bank_train$y = as.numeric(bank_train$y == "yes")
glimpse(bank_train)

bank_train$y = as.factor(bank_train$y)
glimpse(bank_train)


##-----------------------Decision Tree model------------------------------------


set.seed(53)
b=sample(1:nrow(bank_train),0.8*nrow(bank_train))
bank_train1=bank_train[b,]
bank_train2=bank_train[-b,]

glimpse(bank_train1)

library(tree)

bank.tree = tree(y~.,data=bank_train1)
View(predict(bank.tree,newdata = bank_train2,type='vector'))
bank.tree.score=predict(bank.tree,newdata = bank_train2,type='vector')[,2]
View(bank.tree.score)
pROC::roc(bank_train2$y,bank.tree.score)$auc
## Area under the curve: 0.8377

##Now we know the probable performance , we can go ahead and build the model on entire training data

bank.tree.final=tree(y~.,data=bank_train)
bank.tree.final.score=predict(bank.tree.final,newdata = bank_test,type='vector')[,2]
View(predict(bank.tree.final,newdata = bank_test,type='vector'))

write.table(bank.tree.final.score,"padmaja_Saripalli_P5_part2_tree.csv",col.names = c("y"),row.names = F)

## However if we needed to submit hard classes, we'll need to determine cutoff on the probability scores

train.score=predict(bank.tree.final,newdata=bank_train,type='vector')[,2]
View(train.score)
real=bank_train$y
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
## max KS = 0.5345947

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
my_cutoff

KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff
## [1] 0.101
##now that you have your cutoff, you can make hard predictions

test.predicted=as.numeric(bank.tree.final.score>my_cutoff)
write.table(test.predicted,"padmaja_Saripalli_P5_part2_tree_hardclasses.csv",col.names = c("y"),row.names = F)


##------------------Random Forest model--------------------------------------------------------


param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}

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
  k=cvTuning(randomForest,y~.,
             data =bank_train,
             tuning =params,
             folds = cvFolds(nrow(bank_train), K=10, type ="random"),
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
## 0.8112184
best_params


bank.rf.final=randomForest(y~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=bank_train)


View(predict(bank.rf.final,newdata = bank_test,type='prob'))
test.score=predict(bank.rf.final,newdata = bank_test,type='prob')[,2]
write.table(test.score,'padmaja_Saripalli_P5_part2_rf.csv',col.names = c("y"),row.names = F)

##To create hard classes 


train.score=predict(bank.rf.final,newdata=bank_train,type='prob')[,2]
View(train.score)
real=bank_train$y
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
## max KS = 0.5850571
my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
my_cutoff
##  0.001

##now that you have your cutoff, you can make hard predictions

test.predicted=as.numeric(test.score>my_cutoff)
View(test.predicted)
write.table(test.predicted,"padmaja_Saripalli_P5_part2_rf_hardclasses.csv",col.names = c("y"),row.names = F)
