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

##------------------------------------------------------------------------------------------

##  Project 4 Part1 quiz


## 2. Find out total promotions happened in last 5 years

glimpse(HR_full)

table(HR_train$promotion_last_5years)   ## 228

## Find out the variance in statisfaction_level for category 0 of variable 'left' 
##  (round off to 4 decimal places).

var(table(HR_train$satisfaction_level,HR_train$left))
round(tapply(HR_train$satisfaction_level,HR_train$left,var),4)
##  0.0487
##  Does average_monthly_hours follow normal distribution?

set.seed(7)
s=sample(1:nrow(HR_full),0.3*nrow(HR_full))
HR_train1=HR_full[s,]
HR_train2=HR_full[-s,]
shapiro.test(HR_train1$average_montly_hours)

glimpse(HR_train1)


##  Find out which category of salary has maximum employee resignation.

tapply(HR_train$salary,HR_train$left,max)

table(HR_train$salary & HR_train$left ==1)

## Find out correlation coefficient between last_evaluation and average_monthly_hours 
## (round it off to 2 decimal places).

round(cor(HR_full$last_evaluation,HR_full$average_montly_hours),2)

##  According to given data what is the probability that someone will leave the organisation 
## if they were involved in a work accident? (round off 2 decimal places)

prop.table(table(HR_train$Work_accident,HR_train$left),1)


##What is the median time spent with the company among people leaving the company?

glimpse(HR_train)

ts = HR_train$time_spend_company[HR_train$left==1]
median(ts)

##  Which sales category has maximum median average_monthly_hours?

HR_train %>%
  group_by(sales) %>%
  summarise(medave = median(average_montly_hours)) %>%
  arrange(desc(medave))

# Does number of projects significantly differ between two categories
# of the target variable "left"?

glimpse(HR_test)

table(HR_train$number_project,HR_train$left)  #yes

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



##-----------------------------------------------------------------------------------------
##  --------------------------Logistic Regression--------------------------------------
##--------------------------------------------------------------------------------------------



HR_vif=lm(left~.,data=HR_train1)

sort(vif(HR_vif),decreasing = T)

HR_log=glm(left~., data=HR_train1,family = "binomial")

HR_log = step(HR_log)

formula(HR_log)

HR_log = glm(left ~ satisfaction_level + last_evaluation + number_project + 
               average_montly_hours + time_spend_company + Work_accident + 
               promotion_last_5years + sales_marketing + salary_medium + 
               salary_low, data= HR_train1,family="binomial")

summary(HR_log)
## Dropping the features with higher p-value
HR_log = glm(left ~ satisfaction_level + last_evaluation + number_project + 
               average_montly_hours + time_spend_company + Work_accident + 
               promotion_last_5years + salary_medium + 
               salary_low, data= HR_train1,family="binomial")

summary(HR_log)


library(pROC)

HR.score=predict(HR_log,newdata = HR_train2,type='response')

auc(roc(HR_train2$left,HR.score))

## 0.7211 auc score

# now lets build the model on entire training data

HR_vif_final=lm(left~.,data=HR_train)
sort(vif(HR_vif_final),decreasing = T)

HR_log_final=glm(left~., data=HR_train,family = "binomial")

HR_log_final = step(HR_log_final)

formula(HR_log_final)

HR_log_final = glm(left ~ satisfaction_level + last_evaluation + number_project + 
                     average_montly_hours + time_spend_company + Work_accident + 
                     promotion_last_5years + sales_hr + sales_accounting + sales_marketing + 
                     sales_support + sales_technical + sales_sales + salary_medium + 
                     salary_low, data=HR_train, family = "binomial")
summary(HR_log_final)

HR_log_final = glm(left ~ satisfaction_level + last_evaluation + number_project + 
                     average_montly_hours + time_spend_company + Work_accident + 
                     promotion_last_5years + sales_hr + sales_accounting + sales_marketing + 
                     sales_support + sales_technical + salary_medium + 
                     salary_low, data=HR_train, family = "binomial")
summary(HR_log_final)

HR_log_final = glm(left ~ satisfaction_level + last_evaluation + number_project + 
                     average_montly_hours + time_spend_company + Work_accident + 
                     promotion_last_5years + sales_hr + sales_accounting + sales_marketing + 
                     sales_technical + salary_medium + 
                     salary_low, data=HR_train, family = "binomial")
summary(HR_log_final)

# now if we needed to submit probability scores for the test data we can do at this point

HRlog.prob.score= predict(HR_log_final,newdata = HR_test,type='response')
write.csv(HRlog.prob.score,"padmaja_Saripalli_P4_part2.csv",row.names = F)


# however if we need to submit hard classes, we'll need to determine cutoff score

train.score=predict(HR_log_final,newdata = HR_train,type='response')

real=HR_train$left
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
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]


#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=M))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)



ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

# now that we have our cutoff we can convert score to hard classes

test.predicted=as.numeric(HRlog.prob.score>my_cutoff)
write.csv(test.predicted,"padmaja_Saripalli_P4_part2_hardclasses.csv",row.names = F)

##-------------------------Decision Tree model--------------------------------------------

library(tree)
library(cvTools)
library(randomForest)
library(ggplot2)

## For classification tree we'll need to convert response to factor type

HR_train$left=as.factor(HR_train$left)
HR_train1$left = as.factor(HR_train1$left)
HR_train2$left = as.factor(HR_train2$left)
glimpse(HR_train)
glimpse(HR_train1)
glimpse(HR_train2)

HR.tree=tree(left~.,data=HR_train1)

## Visual Format

plot(HR.tree)
text(HR.tree)


## Performance on validation set

HRtree.score=predict(HR.tree,newdata = HR_train2,type='vector')[,2]
pROC::roc(HR_train2$left,HRtree.score)$auc
##Area under the curve: 0.8284

## build model on entire data

HR.tree.final=tree(left~.,
                   data=HR_train)

plot(HR.tree.final)
text(HR.tree.final)

## Probability score prediction on test/production data

HRtest.tree.score=predict(HR.tree.final,newdata=HR_test,type='vector')[,2]
View(HRtest.tree.score)
write.csv(HRtest.tree.score,"padmaja_Saripalli_P4_part2_tree.csv",row.names = F)

## For hardclass prediction we'll need to find a cutoff on score
## Process is same as for logistic regression

train.score=predict(HR.tree.final,newdata=HR_train,type='vector')[,2]
real=HR_train$left

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

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff
## 0.179
## Once we know the cutoff we can use it to convert test score to 
## hard classes

HRtreetest.predicted=as.numeric(HRtest.tree.score>my_cutoff)
write.csv(HRtreetest.predicted,"padmaja_Saripalli_P4_part2_tree_hardclasses.csv",row.names = F)


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

param=list(mtry=c(5,10,15,20),
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
##0.8410535
##  0.8401224

HR.rf.final=randomForest(left~.,
                        mtry=best_params$mtry,
                        ntree=best_params$ntree,
                        maxnodes=best_params$maxnodes,
                        nodesize=best_params$nodesize,
                        data=HR_train
)


HRtest.rf.score=predict(HR.rf.final,newdata = HR_test,type='prob')[,2]
write.table(HRtest.rf.score,'padmaja_Saripalli_P4_Part2_rf.csv',row.names = F)

## However if we needed to submit hard classes, we'll need to determine cutoff on the probability scores

train.score=predict(HR.rf.final,newdata=HR_train,type='prob')[,2]
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

##---------------------------------------------------------------------------
# for gbm also tune the parameter bag.fraction , add it to param list ,
# values to try : c(0.5,0.6,0.7,0.8,0.9,1)

## ------------------------------------------------------------------------
param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10),
           bag.fraction=c(0.5,0.6,0.7,0.8,0.9,1))

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

