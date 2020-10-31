setwd("D:\\Padmaja\\EdvancerClass\\R\\RetailProject")
getwd()

S_train=read.csv("D:\\Padmaja\\EdvancerClass\\R\\RetailProject\\store_train.csv", stringsAsFactors = F)
S_test = read.csv("D:\\Padmaja\\EdvancerClass\\R\\RetailProject\\store_test.csv", stringsAsFactors = F)

library(dplyr)

glimpse(S_train)
glimpse(S_test)

View(S_train)


S_test$store = NA
glimpse(S_test)

S_train$tdata = "train"
S_test$tdata = "test"
glimpse(S_train)
glimpse(S_test)

S_all = rbind(S_train,S_test)

glimpse(S_all)

###----------------------------------------------------------------------------------
#Project 2 Part1 Quiz

# 1. what is the total sales (sum of all sales) of 
##  Supermarket Type1 in area Kennebec County, ME?


glimpse(ts)

data % > %
  group_by(yearID, teamID) % > %
  summarise(mean_games = mean(G)) % > %
  arrange(desc(teamID, yearID))

dat3 = S_all %>%
  group_by(store_Type,Areaname) %>%
  select(Id,starts_with("sales"), Areaname, store_Type)


View(dat3)
install.packages("sqldf")
library(sqldf)

dat4 <- sqldf("select * from dat3 where store_Type='Supermarket Type1' and Areaname = 'Kennebec County, ME'")
sum(dat4$sales0)+sum(dat4$sales1)+sum(dat4$sales2)+sum(dat4$sales3)+sum(dat4$sales4)
##  69624, 3868(add only one row of similar sales)

dat1 = S_all %>%
  group_by(store_Type,Areaname) %>%
  summarise(Tsales0 = sum(sales0),Tsales1 = sum(sales1), Tsales2 = sum(sales2),Tsales3=sum(sales3),Tsales4=sum(sales4))

View(S_all)
dat1[dat1$store_Type=="Supermarket Type1" & dat1$Areaname == "Kennebec County, ME",]

#  1 Supermarket Type1 Kennebec County, ME   13788    9324   10782   17298   18432

#-69,624


## 4.  Find out number of unique categories of variable Areaname.
length(unique(table(S_all$Areaname)))

prop.table(table(S_all$Areaname,S_all$store),1)[]

## 5. For store type grocery store what is the response rate ?
#[ what % of obs have response value as 1 ]  Round off to two decimal digits. 

#Note : Answer needs to be in hundreds . Ex : 12.34 ( NOT 0.1234 )


round(prop.table(table(S_all$store_Type,S_all$store),1),2)


#  Do all the sales variable follow normal distribution?
#Note : Just write 'Yes' or 'No' . If you write sentences , 
#automated grading will consider it incorrect . Answers are not case sensitive . 

## No

# Density plot and Q-Q plot can be used to check normality visually.
# 
# Density plot: the density plot provides a visual judgement about
# whether the distribution is bell shaped.
# install.packages("ggpubr")


library("ggpubr")
ggdensity(S_train$sales0, 
          main = "Density plot sales0",
          xlab = "Sales0")
ggdensity(S_train$sales1, 
          main = "Density plot sales1",
          xlab = "Sales1")
ggdensity(S_train$sales2, 
          main = "Density plot sales2",
          xlab = "Sales2")
ggdensity(S_train$sales3, 
          main = "Density plot sales3",
          xlab = "Sales3")
ggdensity(S_train$sales4, 
          main = "Density plot sales4",
          xlab = "Sales4")

# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between
# a given sample and the normal distribution. 
# A 45-degree reference line is also plotted.

library("car")
qqPlot(S_train$sales0)
qqPlot(S_train$sales1)
qqPlot(S_train$sales2)
qqPlot(S_train$sales3)
qqPlot(S_train$sales4)


# Normality test:
#   
# Visual inspection, described in the previous section, is usually unreliable. 
# It's possible to use a significance test comparing the sample distribution 
# to a normal one in order to ascertain whether data show or not a serious deviation 
# from normality.
# 
# There are several methods for normality test such as Kolmogorov-Smirnov (K-S) normality test 
# and Shapiro-Wilk's test.
# 
# The null hypothesis of these tests is that "sample distribution is normal". 
# If the test is significant, the distribution is non-normal.
# 
# Shapiro-Wilk's method is widely recommended for normality test and it provides better power 
# than K-S. It is based on the correlation between the data and the corresponding 
# normal scores.Note that, normality test is sensitive to sample size. 
# Small samples most often pass normality tests. Therefore, it's important to combine visual 
# inspection and significance test in order to take the right decision.
# 
# The R function shapiro.test() can be used to perform the Shapiro-Wilk test of normality
# for one variable (univariate):
# If the output,of Shapiro wilk the p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. In other words, we can assume 
# the normality.
# 
set.seed(5)
s=sample(1:nrow(S_all),0.4*nrow(S_all))
S_train1=S_all[s,]
S_train2=S_all[-s,]
shapiro.test(S_train1$sales0)
shapiro.test(S_train1$sales1)


# Number of outliers for total sales based on following limits (q1-1.5*IQR, q3+1.5*IQR)?
sales = S_all[,c("sales0","sales1","sales2","sales3","sales4")]
TCsales = apply(sales,2, sum)
TRsales = apply(sales,1,sum)

dat4 = S_all %>%
    mutate(Tsales = TRsales)
glimpse(dat4)

dat5 = dat4[,c("sales0","sales1","sales2","sales3","sales4","population","Tsales")]
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



## returns the upper and lower outlier values for each column
glimpse(dat4)

apply(dat4[,c("sales0","sales1","sales2","sales3","sales4","population","Tsales")],2,outlier_upper)
apply(dat4[,c("sales0","sales1","sales2","sales3","sales4","population","Tsales")],2,outlier_lower)

## Calculate the outliers for all the numeric columns

apply(dat5,2,function(x) sum(x>outlier_upper(x)))
apply(dat5,2,function(x) sum(x<outlier_lower(x)))

###  sales0     sales1     sales2     sales3     sales4 population     Tsales 
###   191        282        187        214         83         NA        191 

###    sales0     sales1     sales2     sales3     sales4 population     Tsales 
###       0          0          0          0          0         NA          0 

tot_sales = S_all %>%
    summarise(Tsales0 = sum(sales0),Tsales1 = sum(sales1), Tsales2 = sum(sales2),Tsales3=sum(sales3),Tsales4=sum(sales4))

##  which store type has maximum variance in total sales?

dat2 = S_all %>%
  group_by(store_Type) %>%
  summarise(Tsales0 = sum(sales0),Tsales1 = sum(sales1), Tsales2 = sum(sales2),Tsales3=sum(sales3),Tsales4=sum(sales4))
glimpse(dat2)
##  Second method
tapply(dat4$Tsales,dat4$store_Type,var)
##  Answer Grocery Store

#var.dat <-sort(apply(dat2[,2:6], MARGIN=1, FUN=var), decreasing = T)

## How many dummies will you create for variable state_alpha?

length(unique(S_all$state_alpha))

prop.table(table(S_all$state_alpha,S_all$store),1)

###   ------------Project2 Part1 quiz End-------------------------------------------

####----------------------------------------------------------------------------------


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


glimpse(S_all)

# table(S_all$countyname)
# 
# table(S_all$Areaname)
# 
# glimpse(S_all)
# sort(table(S_all$state_alpha))
# 
# SME = S_all[S_all$state_alpha == 'ME',]
# table(SME$countyname)



sort(table(S_all$state_alpha))


S_all = CreateDummies(S_all,"state_alpha",30)
glimpse(S_all)


table(S_all$store_Type)
S_all=CreateDummies(S_all,"store_Type",490)
glimpse(S_all)

sort(table(S_all$countyname),decreasing = T)
S_all=CreateDummies(S_all,"countyname",8)
glimpse(S_all)

## After installing the MASS package there will be a clash with the dplyr::select function. 
##so we have to specify dplyr::select to make the select function work

library(dplyr)
S_all=S_all %>%
  dplyr::select(-countytownname,-Areaname,-storecode,-CouSub,-country,-State)

glimpse(S_all)

# zScale <- function(myVar){
#  zVar = (myVar - mean(myVar)) / sd(myVar)
#  return(zVar)
#   
# }
# 
# ?apply
# apply(S_all, MARGIN = 2, zScale(S_all$sales0))

install.packages("clusterSim")
library(clusterSim)

S_all$sales0 <- data.Normalization(S_all$sales0,type = "n1", normalization = "column", na.rm=FALSE)
S_all$sales1 <- data.Normalization(S_all$sales1,type = "n1", normalization = "column", na.rm=FALSE)
S_all$sales2 <- data.Normalization(S_all$sales2,type = "n1", normalization = "column", na.rm=FALSE)
S_all$sales3 <- data.Normalization(S_all$sales3,type = "n1", normalization = "column", na.rm=FALSE)
S_all$sales4 <- data.Normalization(S_all$sales4,type = "n1", normalization = "column", na.rm=FALSE)


# that tells us that there are no character columns remaining [ 1 comes for column 'data']

sum(sapply(S_all,function(x) is.character(x)))


# Find all na's

lapply(S_all,function(x) sum(is.na(x)))

for(col in names(S_all)){
  
  if(sum(is.na(S_all[,col]))>0 & !(col %in% c("tdata","store"))){
    
    S_all[is.na(S_all[,col]),col]=mean(S_all[S_all$tdata=='train',col],na.rm=T)
  }
  
}

S_all$population <- data.Normalization(S_all$population,type = "n1", normalization = "column", na.rm=TRUE)
glimpse(S_all)

S_train = S_all %>% filter(tdata=='train') %>% dplyr::select(-tdata)
S_test = S_all %>% filter(tdata=='test') %>% dplyr::select (-tdata,-store)

glimpse(S_train)

glimpse(S_test)


set.seed(22)
s=sample(1:nrow(S_train),0.8*nrow(S_train))
S_train1=S_train[s,]
S_train2=S_train[-s,]
glimpse(S_train1)

library(car)

Svif=lm(store~.-Id,data=S_train1)

sort(vif(Svif),decreasing = T)

Slog = glm(store~.-Id,data=S_train1,family = "binomial")

Slog = step(Slog)


formula(Slog)


Slog = glm(store ~ sales1 + sales2 + sales3 + sales4 + population + state_alpha_RI +
             state_alpha_ID + state_alpha_SC + state_alpha_WV + state_alpha_NY +
             state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL +
             state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK +
             state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH +
             state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC +
             state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY +
             state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT +
             state_alpha_NH + state_alpha_ME + countyname_LamoilleCounty +
             countyname_CalhounCounty + countyname_SagadahocCounty + countyname_AdamsCounty +
             countyname_StraffordCounty + countyname_AndroscogginCounty +
             countyname_BenningtonCounty + countyname_CaledoniaCounty +
             countyname_MadisonCounty + countyname_SullivanCounty + countyname_HampshireCounty +
             countyname_NewLondonCounty + countyname_BristolCounty + countyname_CheshireCounty +
             countyname_HampdenCounty + countyname_NorfolkCounty + countyname_PlymouthCounty +
             countyname_RutlandCounty + countyname_HartfordCounty + countyname_BerkshireCounty +
             countyname_HillsboroughCounty + countyname_YorkCounty + countyname_CumberlandCounty +
             countyname_RockinghamCounty + countyname_CoosCounty + countyname_WorcesterCounty +
             countyname_PenobscotCounty + countyname_AroostookCounty +
             countyname_FranklinCounty, data=S_train1, family = "binomial")

summary(Slog)

Slog = glm(store ~ sales1 + sales2 + sales3 + sales4 + population + state_alpha_RI +
             state_alpha_ID + state_alpha_SC + state_alpha_WV + state_alpha_NY +
             state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL +
             state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK +
             state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH +
             state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC +
             state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY +
             state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT +
             state_alpha_NH + state_alpha_ME +
             countyname_CalhounCounty + countyname_SagadahocCounty +
             countyname_StraffordCounty + countyname_AndroscogginCounty +
             countyname_HampshireCounty +
             countyname_NewLondonCounty +
             countyname_HampdenCounty + countyname_PlymouthCounty +
             countyname_HartfordCounty + countyname_BerkshireCounty +
             countyname_HillsboroughCounty + countyname_YorkCounty + countyname_CumberlandCounty +
             countyname_RockinghamCounty + countyname_CoosCounty + countyname_WorcesterCounty +
             countyname_PenobscotCounty + countyname_AroostookCounty +
             countyname_FranklinCounty, data=S_train1, family = "binomial")

summary(Slog)

# Slog = glm(store ~ sales1 + sales2 + sales3 + sales4 + population + state_alpha_RI + 
#              state_alpha_ID + state_alpha_SC + state_alpha_WV + state_alpha_NY + 
#              state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
#              state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
#              state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
#              state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC + 
#              state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
#              state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
#              state_alpha_NH + state_alpha_ME + countyname_LamoilleCounty + 
#              countyname_CalhounCounty + countyname_SagadahocCounty + countyname_AdamsCounty + 
#              countyname_StraffordCounty + countyname_AndroscogginCounty + 
#              countyname_BenningtonCounty + countyname_CaledoniaCounty + 
#              countyname_MadisonCounty + countyname_SullivanCounty + countyname_HampshireCounty + 
#              countyname_NewLondonCounty + countyname_BristolCounty + countyname_CheshireCounty + 
#              countyname_HampdenCounty + countyname_NorfolkCounty + countyname_PlymouthCounty + 
#              countyname_RutlandCounty + countyname_HartfordCounty + countyname_BerkshireCounty + 
#              countyname_HillsboroughCounty + countyname_YorkCounty + countyname_CumberlandCounty + 
#              countyname_RockinghamCounty + countyname_CoosCounty + countyname_WorcesterCounty + 
#              countyname_PenobscotCounty + countyname_AroostookCounty + 
#              countyname_FranklinCounty, data=S_train1, family = "binomial")
# summary(Slog)
# 
# Slog = glm(store ~ sales1 + sales2 + sales3 + sales4 + population + state_alpha_RI + 
#              state_alpha_ID + state_alpha_SC + state_alpha_WV + state_alpha_NY + 
#              state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
#              state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
#              state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
#              state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC + 
#              state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
#              state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
#              state_alpha_NH + state_alpha_ME + 
#              countyname_CalhounCounty + countyname_SagadahocCounty + 
#              countyname_StraffordCounty + countyname_AndroscogginCounty + 
#              countyname_HampshireCounty + 
#              countyname_NewLondonCounty + 
#              countyname_HampdenCounty + countyname_PlymouthCounty + 
#              countyname_HartfordCounty + countyname_BerkshireCounty + 
#              countyname_HillsboroughCounty + countyname_YorkCounty + countyname_CumberlandCounty + 
#              countyname_RockinghamCounty + countyname_CoosCounty + countyname_WorcesterCounty + 
#              countyname_PenobscotCounty + countyname_AroostookCounty + 
#              countyname_FranklinCounty, data=S_train1, family = "binomial")
# summary(Slog)

library(pROC)

val.score=predict(Slog,newdata = S_train2,type='response')

auc(roc(S_train2$store,val.score))
## Area under the curve: 0.8019

# so the tentative score performance of logistic regression is going to be around 
#Area under the curve: 0.8019
# now lets build the model on entire training data


Svif_final=lm(store~.-Id,data=S_train)

sort(vif(Svif_final),decreasing = T)

Slog_final = glm(store~.-Id,data=S_train,family = "binomial")

Slog_final = step(Slog_final)


formula(Slog_final)

Slog_final = glm(store ~ sales1 + sales2 + sales4 + population + state_alpha_RI + 
                   state_alpha_ID + state_alpha_SC + state_alpha_WV + state_alpha_NY + 
                   state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
                   state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
                   state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_MN + 
                   state_alpha_OH + state_alpha_IN + state_alpha_NE + state_alpha_TN + 
                   state_alpha_IA + state_alpha_NC + state_alpha_IL + state_alpha_KS + 
                   state_alpha_MO + state_alpha_KY + state_alpha_VA + state_alpha_GA + 
                   state_alpha_TX + state_alpha_VT + state_alpha_NH + state_alpha_MA + 
                   state_alpha_ME + countyname_HenryCounty + countyname_LamoilleCounty + 
                   countyname_CalhounCounty + countyname_LawrenceCounty + countyname_SagadahocCounty + 
                   countyname_AdamsCounty + countyname_LakeCounty + countyname_StraffordCounty + 
                   countyname_AndroscogginCounty + countyname_BenningtonCounty + 
                   countyname_CaledoniaCounty + countyname_OrleansCounty + countyname_SullivanCounty + 
                   countyname_HampshireCounty + countyname_NewLondonCounty + 
                   countyname_BristolCounty + countyname_CheshireCounty + countyname_HampdenCounty + 
                   countyname_PlymouthCounty + countyname_RutlandCounty + countyname_HartfordCounty + 
                   countyname_BerkshireCounty + countyname_HillsboroughCounty + 
                   countyname_YorkCounty + countyname_CumberlandCounty + countyname_RockinghamCounty + 
                   countyname_CoosCounty + countyname_EssexCounty + countyname_PenobscotCounty + 
                   countyname_AroostookCounty + countyname_FranklinCounty + 
                   countyname_WashingtonCounty, data=S_train, family="binomial")

summary(Slog_final)

Slog_final = glm(store ~ sales1 + sales2 + sales4 + population + state_alpha_RI + 
                   state_alpha_ID + state_alpha_SC + state_alpha_WV + state_alpha_NY + 
                   state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
                   state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
                   state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_MN + 
                   state_alpha_OH + state_alpha_IN + state_alpha_NE + state_alpha_TN + 
                   state_alpha_IA + state_alpha_NC + state_alpha_IL + state_alpha_KS + 
                   state_alpha_MO + state_alpha_KY + state_alpha_VA + state_alpha_GA + 
                   state_alpha_TX + state_alpha_VT + state_alpha_NH + state_alpha_MA + 
                   state_alpha_ME + 
                   countyname_CalhounCounty + countyname_SagadahocCounty + 
                   countyname_StraffordCounty + 
                   countyname_AndroscogginCounty + 
                   countyname_OrleansCounty + 
                   countyname_HampshireCounty + countyname_NewLondonCounty + 
                   countyname_HartfordCounty + 
                   countyname_BerkshireCounty + countyname_HillsboroughCounty + 
                   countyname_YorkCounty + countyname_CumberlandCounty + countyname_RockinghamCounty + 
                   countyname_CoosCounty + countyname_EssexCounty + countyname_PenobscotCounty + 
                   countyname_AroostookCounty + countyname_FranklinCounty + 
                   countyname_WashingtonCounty, data=S_train, family="binomial")

summary(Slog_final)

# Slog_final = glm(store ~ sales1 + sales2 + sales4 + population + state_alpha_RI + 
#                    state_alpha_ID + state_alpha_SC + state_alpha_WV + state_alpha_NY + 
#                    state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
#                    state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
#                    state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_MN + 
#                    state_alpha_OH + state_alpha_IN + state_alpha_NE + state_alpha_TN + 
#                    state_alpha_IA + state_alpha_NC + state_alpha_IL + state_alpha_KS + 
#                    state_alpha_MO + state_alpha_KY + state_alpha_VA + state_alpha_GA + 
#                    state_alpha_TX + state_alpha_VT + state_alpha_NH + state_alpha_MA + 
#                    state_alpha_ME + countyname_HenryCounty + countyname_LamoilleCounty + 
#                    countyname_CalhounCounty + countyname_LawrenceCounty + countyname_SagadahocCounty + 
#                    countyname_AdamsCounty + countyname_LakeCounty + countyname_StraffordCounty + 
#                    countyname_AndroscogginCounty + countyname_BenningtonCounty + 
#                    countyname_CaledoniaCounty + countyname_OrleansCounty + countyname_SullivanCounty + 
#                    countyname_HampshireCounty + countyname_NewLondonCounty + 
#                    countyname_BristolCounty + countyname_CheshireCounty + countyname_HampdenCounty + 
#                    countyname_PlymouthCounty + countyname_RutlandCounty + countyname_HartfordCounty + 
#                    countyname_BerkshireCounty + countyname_HillsboroughCounty + 
#                    countyname_YorkCounty + countyname_CumberlandCounty + countyname_RockinghamCounty + 
#                    countyname_CoosCounty + countyname_EssexCounty + countyname_PenobscotCounty + 
#                    countyname_AroostookCounty + countyname_FranklinCounty + 
#                    countyname_WashingtonCounty, data=S_train, family="binomial")
# 
# summary(Slog_final)
# 
# Slog_final = glm(store ~ sales1 + sales2 + sales4 + population + state_alpha_RI + 
#                    state_alpha_ID + state_alpha_SC + state_alpha_WV + state_alpha_NY + 
#                    state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
#                    state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
#                    state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_MN + 
#                    state_alpha_OH + state_alpha_IN + state_alpha_NE + state_alpha_TN + 
#                    state_alpha_IA + state_alpha_NC + state_alpha_IL + state_alpha_KS + 
#                    state_alpha_MO + state_alpha_KY + state_alpha_VA + state_alpha_GA + 
#                    state_alpha_TX + state_alpha_VT + state_alpha_NH + state_alpha_MA + 
#                    state_alpha_ME + 
#                    countyname_CalhounCounty + countyname_SagadahocCounty + 
#                    countyname_StraffordCounty + 
#                    countyname_AndroscogginCounty + 
#                    countyname_OrleansCounty + 
#                    countyname_NewLondonCounty + 
#                    countyname_HartfordCounty + 
#                    countyname_BerkshireCounty + countyname_HillsboroughCounty + 
#                    countyname_YorkCounty + countyname_CumberlandCounty + countyname_RockinghamCounty + 
#                    countyname_CoosCounty + countyname_EssexCounty + countyname_PenobscotCounty + 
#                    countyname_AroostookCounty + countyname_FranklinCounty, data=S_train, family="binomial")

summary(Slog_final)


# now if we needed to submit probability scores for the test data we can do at this point

test.prob.score= predict(Slog_final,newdata = S_test,type='response')
write.table(test.prob.score,"padmaja_Saripalli_P2_part2.csv",col.names = c("store"),row.names = F)

# however if we need to submit hard classes, we'll need to determine cutoff score

train.score=predict(Slog_final,newdata = S_train,type='response')

real=S_train$store
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
## 0.379
# now that we have our cutoff we can convert score to hard classes

test.predicted=as.numeric(test.prob.score>my_cutoff)
write.table(test.predicted,"padmaja_Saripalli_P2_part2_hardclasses.csv",col.names = c("store"),row.names = F)


###------------------------------------------------------------------------------------
###--------------------------Decision Tree Model----------------------------------------
###------------------------------------------------------------------------------------

## First we have to convert the response variable to a factor if its a classification problem

S_train$store=as.factor(S_train$store)

glimpse(S_train)
glimpse(S_test)
set.seed(23)
s=sample(1:nrow(S_train),0.8*nrow(S_train))
S_train1=S_train[s,]
S_train2=S_train[-s,]

glimpse(S_train2)

library(tree)

S.tree=tree(store~.-Id,data=S_train1)

Stree.score=predict(S.tree,newdata = S_train2,type='vector')[,2]
View(Stree.score)
pROC::roc(S_train2$store,Stree.score)$auc
##Area under the curve: 0.7278

##Now we know the probable performance , we can go ahead and build the model on entire training data

S.tree.final=tree(store~.-Id,data=S_train)
Stree.final.score=predict(S.tree.final,newdata = S_test,type='vector')[,2]
View(predict(S.tree.final,newdata = S_test,type='vector'))

write.table(Stree.final.score,"padmaja_Saripalli_P2_Part2_tree.csv",col.names=c("store"),row.names = F)

## However if we needed to submit hard classes, we'll need to determine cutoff on the probability scores

train.score=predict(S.tree.final,newdata=S_train,type='vector')[,2]
View(train.score)
real=S_train$store
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
## [1] 0.235
##now that you have your cutoff, you can make hard predictions

test.predicted=as.numeric(Stree.final.score>my_cutoff)
write.table(test.predicted,"padmaja_Saripalli_P2_Part2_tree_hardclasses.csv",col.names = c("store"),row.names = F)




##---------------------------------------------------------------------------------------
##---------------------------Random Forest Model-----------------------------------------
##---------------------------------------------------------------------------------------

library(randomForest)
library(cvTools)

glimpse(S_train)
glimpse(S_test)

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
  k=cvTuning(randomForest,store~.-Id,
             data =S_train,
             tuning =params,
             folds = cvFolds(nrow(S_train), K=10, type ="random"),
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
## 0.8045753
best_params

S.rf.final=randomForest(store~.-Id,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=S_train
)


test.rf.score=predict(S.rf.final,newdata = S_test,type='prob')[,2]
write.table(test.rf.score,'padmaja_Saripalli_P2_Part2_rf.csv',col.names = c("store"),row.names = F)

## However if we needed to submit hard classes, we'll need to determine cutoff on the probability scores

train.score=predict(S.rf.final,newdata=S_train,type='prob')[,2]
View(train.score)
real=S_train$store
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
##0.13
##now that you have your cutoff, you can make hard predictions

test.predicted=as.numeric(test.rf.score>my_cutoff)
write.table(test.predicted,"padmaja_Saripalli_P2_Part2_rf_hardclasses.csv",col.names = c("store"),row.names = F)


