
setwd("D:\\Padmaja\\EdvancerClass\\R\\RealEstateProject")
getwd()

RE_train = read.csv("housing_train.csv", stringsAsFactors = F)
RE_test = read.csv("housing_test.csv", stringsAsFactors = F)

View(RE_train)

library(dplyr)
##---------------------------------------------------------------------------------
##---------------------------------------------------------------------------------

### Project Part 1

var(RE_train$Price, na.rm = T)


length(is.na(RE_all$YearBuilt))

lapply(RE_all,function(x) sum(is.na(x)))

glimpse(RE_all)
reh = RE_all[RE_all$Type == "h",]
glimpse(reh)

m1 = mean(reh$Price,na.rm = T)

ret = RE_all[RE_all$Type == "t",]
glimpse(ret)
m2 = mean(ret$Price,na.rm = T) 
m1-m2

length(unique(RE_all$Postcode))

hist(RE_all$Distance, 100, col="black")
plot(RE_all$Distance)

RES <- summarise(group_by(RE_all,SellerG), sum(Price, na.rm = T))

RES[RES$`sum(Price, na.rm = T)`== 1062676265,]

sort(RES$`sum(Price, na.rm = T)`,decreasing = T)

glimpse(RES)
RE_CA = summarise(group_by(RE_all,CouncilArea), var(Price, na.rm = T))
sort(RE_CA$`var(Price, na.rm = T)`,decreasing = T)
RE_CA[RE_CA$`mean(Price, na.rm = T)`== 1692174.3,]
library(ggplot2)
p=ggplot(RE_all$Distance,aes(x=X))
p+geom_bar(aes(y=(..count..)/sum(..count..)))+ylab("Frequency Percent")
glimpse(RE_CA)

set.seed(4)
s=sample(1:nrow(RE_train),0.6*nrow(RE_train))
RE_train1=RE_train[s,]
RE_train2=RE_train[-s,]
shapiro.test(RE_train1$Distance)

### Project Part 1 end

##--------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------


### Project Part 2

glimpse(RE_train)

glimpse(RE_test)

View(RE_train)
RE_test$Price = NA

RE_train$data = 'train'
RE_test$data = 'test'

RE_all = rbind(RE_train,RE_test)

View(RE_all)
table(RE_all$Suburb,RE_all$CouncilArea)
table(RE_all$Suburb)

RE_all$Suburb_CouncilArea=paste(RE_all$Suburb,RE_all$CouncilArea,sep="_")
RE_all$Suburb = NULL
RE_all$CouncilArea = NULL
# drop amount funded by investor

length(table(RE_all$SellerG))
length(table(trimws(RE_all$Address)))


glimpse(RE_all)



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
    name=gsub("\\/","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


glimpse(RE_all$Postcode)



RE_all$Address =gsub("[0-9.-]", "", RE_all$Address)
table(trimws(RE_all$Address))



glimpse(RE_all)

sort(table(RE_all$Suburb_CouncilArea), decreasing = T)
RE_all = CreateDummies(RE_all,"Suburb_CouncilArea",30)
glimpse(RE_all)

sort(table(RE_all$Type), decreasing = T)

RE_all = CreateDummies(RE_all,"Type",300)
glimpse(RE_all)

# RE_all = CreateDummies(RE_all,"SellerG",2)
# glimpse(RE_all)
# 
# RE_all = CreateDummies(RE_all,"Address",20)


sort(table(RE_all$Method), decreasing = T)

RE_all = CreateDummies(RE_all,"Method",30)
glimpse(RE_all)



# sort(table(RE_all$CouncilArea), decreasing = T)
# 
# RE_all = CreateDummies(RE_all,"CouncilArea",60)
# glimpse(RE_all)

lapply(RE_all,function(x) sum(is.na(x)))

for(col in names(RE_all)){
  
  if(sum(is.na(RE_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    RE_all[is.na(RE_all[,col]),col]=mean(RE_all[,col],na.rm=T)
  }
  
}

glimpse(RE_all)

##  Remove Address,Postcode and SellerG columns

RE_all$Address <- NULL
RE_all$SellerG <- NULL


glimpse(RE_all)

## separate train and test

RE_train=RE_all %>% filter(data=='train') %>% dplyr:: select(-data)
RE_test=RE_all %>% filter(data=='test') %>% dplyr:: select(-data,-Price)

library(car)
glimpse(RE_train)
glimpse(RE_test)

set.seed(2)
s=sample(1:nrow(RE_train),0.7*nrow(RE_train))
RE_train1=RE_train[s,]
RE_train2=RE_train[-s,]

REfit=lm(Price~.,data=RE_train1)
sort(vif(REfit),decreasing = T)

REfit=lm(Price~.-Method_S,data=RE_train1)
sort(vif(REfit),decreasing = T)

# REfit=lm(Price~.-Method_S-CouncilArea_ ,data=RE_train1)
# sort(vif(REfit),decreasing = T)

# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode,data=RE_train1)
# sort(vif(REfit),decreasing = T)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance,data=RE_train1)
# sort(vif(REfit),decreasing = T)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson,data=RE_train1)
# sort(vif(REfit),decreasing = T)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson-CouncilArea_Maribyrnong,data=RE_train1)
# sort(vif(REfit),decreasing = T)
# 
# 
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-SellerG_Weda,data=RE_train1)
# 
# 
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda,data=RE_train1)
# sort(vif(REfit),decreasing = T)
?order
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ] 

summary(REfit)

# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21,data=RE_train1)

# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray,data=RE_train1)
# summary(REfit)
# 
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside,data=RE_train1)
# summary(REfit)
# 
# 
# 
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham,data=RE_train1)
# summary(REfit)
# 
# 
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin-Suburb_Yarraville,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin-Suburb_Yarraville-Suburb_MooneePonds,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin-Suburb_Yarraville-Suburb_MooneePonds
#          -Suburb_PascoeVale,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin-Suburb_Yarraville-Suburb_MooneePonds
#          -Suburb_PascoeVale-Suburb_StKilda,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin-Suburb_Yarraville-Suburb_MooneePonds
#          -Suburb_PascoeVale-Suburb_StKilda-Suburb_Brunswick,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin-Suburb_Yarraville-Suburb_MooneePonds
#          -Suburb_PascoeVale-Suburb_StKilda-Suburb_Brunswick-Suburb_Coburg,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin-Suburb_Yarraville-Suburb_MooneePonds
#          -Suburb_PascoeVale-Suburb_StKilda-Suburb_Brunswick-Suburb_Coburg-Suburb_Richmond,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Footscray-CouncilArea_Bayside-CouncilArea_Manningham
#          -Suburb_Northcote-CouncilArea_Darebin-Suburb_Yarraville-Suburb_MooneePonds
#          -Suburb_PascoeVale-Suburb_StKilda-Suburb_Brunswick-Suburb_Coburg-Suburb_Richmond
#          -Suburb_PortMelbourne,data=RE_train1)
# summary(REfit)

# 
REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ] 
#summary(REfit)
REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ] 
#summary(REfit)

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ] 



REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ] 

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]


REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule-Suburb_CouncilArea_Ormond_GlenEira,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule-Suburb_CouncilArea_Ormond_GlenEira
         -Suburb_CouncilArea_BentleighEast_GlenEira,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule-Suburb_CouncilArea_Ormond_GlenEira
         -Suburb_CouncilArea_BentleighEast_GlenEira-Suburb_CouncilArea_AltonaNorth_HobsonsBay,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule-Suburb_CouncilArea_Ormond_GlenEira
         -Suburb_CouncilArea_BentleighEast_GlenEira-Suburb_CouncilArea_AltonaNorth_HobsonsBay
         -Suburb_CouncilArea_KeilorEast_MooneeValley,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule-Suburb_CouncilArea_Ormond_GlenEira
         -Suburb_CouncilArea_BentleighEast_GlenEira-Suburb_CouncilArea_AltonaNorth_HobsonsBay
         -Suburb_CouncilArea_KeilorEast_MooneeValley-Suburb_CouncilArea_CaulfieldSouth_GlenEira,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule-Suburb_CouncilArea_Ormond_GlenEira
         -Suburb_CouncilArea_BentleighEast_GlenEira-Suburb_CouncilArea_AltonaNorth_HobsonsBay
         -Suburb_CouncilArea_KeilorEast_MooneeValley-Suburb_CouncilArea_CaulfieldSouth_GlenEira
         -Suburb_CouncilArea_Elwood_PortPhillip,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule-Suburb_CouncilArea_Ormond_GlenEira
         -Suburb_CouncilArea_BentleighEast_GlenEira-Suburb_CouncilArea_AltonaNorth_HobsonsBay
         -Suburb_CouncilArea_KeilorEast_MooneeValley-Suburb_CouncilArea_CaulfieldSouth_GlenEira
         -Suburb_CouncilArea_Elwood_PortPhillip-Suburb_CouncilArea_Bulleen_Manningham,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]

REfit=lm(Price~.-Method_S-Suburb_CouncilArea_Watsonia_Banyule-Suburb_CouncilArea_Seddon_Maribyrnong
         -Suburb_CouncilArea_Strathmore_MooneeValley-Suburb_CouncilArea_Carnegie_GlenEira
         -Suburb_CouncilArea_BentleighEast_-Suburb_CouncilArea_FitzroyNorth_Yarra
         -Suburb_CouncilArea_Newport_HobsonsBay-Suburb_CouncilArea_MooneePonds_MooneeValley
         -Suburb_CouncilArea_TemplestoweLower_Manningham-Suburb_CouncilArea_Murrumbeena_GlenEira
         -Suburb_CouncilArea_AirportWest_MooneeValley-Suburb_CouncilArea_Fitzroy_Yarra
         -Suburb_CouncilArea_Heidelberg_Banyule-Suburb_CouncilArea_Ormond_GlenEira
         -Suburb_CouncilArea_BentleighEast_GlenEira-Suburb_CouncilArea_AltonaNorth_HobsonsBay
         -Suburb_CouncilArea_KeilorEast_MooneeValley-Suburb_CouncilArea_CaulfieldSouth_GlenEira
         -Suburb_CouncilArea_Elwood_PortPhillip-Suburb_CouncilArea_Bulleen_Manningham-Suburb_CouncilArea_Yarraville_Maribyrnong
         -Suburb_CouncilArea_OakPark_Moreland-Suburb_CouncilArea_Sunshine_Brimbank
         -Suburb_CouncilArea_Burwood_Whitehorse-Suburb_CouncilArea_BrightonEast_Bayside
         -Suburb_CouncilArea_Ivanhoe_Banyule-Suburb_CouncilArea_Niddrie_MooneeValley
         -Suburb_CouncilArea_Hawthorn_Boroondara-Suburb_CouncilArea_Melbourne_Melbourne
         -Suburb_CouncilArea_AltonaNorth_-Suburb_CouncilArea_Rosanna_Banyule-Suburb_CouncilArea_Footscray_Maribyrnong
         -Suburb_CouncilArea_NorthMelbourne_Melbourne-Suburb_CouncilArea_Flemington_MooneeValley
         -Suburb_CouncilArea_Carlton_Melbourne-Suburb_CouncilArea_Bentleigh_
         -Suburb_CouncilArea_Thornbury_Darebin-Suburb_CouncilArea_Maidstone_Maribyrnong
         -Suburb_CouncilArea_CliftonHill_Yarra-Suburb_CouncilArea_PascoeVale_Moreland
         -Suburb_CouncilArea_SunshineNorth_Brimbank-Suburb_CouncilArea_Kensington_Melbourne
         -Suburb_CouncilArea_Elsternwick_GlenEira-Suburb_CouncilArea_GlenIris_Stonnington
         -Suburb_CouncilArea_Glenroy_Moreland-Suburb_CouncilArea_AscotVale_MooneeValley
         -Suburb_CouncilArea_Preston_-Suburb_CouncilArea_Glenroy_-Suburb_CouncilArea_Bentleigh_GlenEira,data=RE_train1)
modcoef <- summary(REfit)[["coefficients"]]

modcoef[order(modcoef[ , 4],decreasing=T), ]
summary(REfit)

# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast,data=RE_train1)
# summary(REfit)
# 
# 
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie,data=RE_train1)
# summary(REfit)
# 
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg--Suburb_Yarraville,data=RE_train1)
# summary(REfit)
# 
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg-Suburb_Yarraville-Suburb_AscotVale,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg-Suburb_Yarraville-Suburb_AscotVale-CouncilArea_Brimbank,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg-Suburb_Yarraville-Suburb_AscotVale
#          -CouncilArea_Brimbank-CouncilArea_Darebin,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg-Suburb_AscotVale-Suburb_Yarraville
#          -CouncilArea_Brimbank-CouncilArea_Darebin-Suburb_MooneePonds,data=RE_train1)
# summary(REfit)
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg-Suburb_AscotVale-Suburb_Yarraville
#          -CouncilArea_Brimbank-CouncilArea_Darebin-Suburb_MooneePonds-CouncilArea_MooneeValley,data=RE_train1)
# summary(REfit)
# 
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg-Suburb_AscotVale-Suburb_Yarraville
#          -CouncilArea_Brimbank-CouncilArea_Darebin-Suburb_MooneePonds-CouncilArea_MooneeValley
#          -Suburb_Thornbury,data=RE_train1)
# summary(REfit)
# 
# 
# REfit=lm(Price~.-Method_S-CouncilArea_ -Suburb_Northcote-Suburb_Bentleigh-CouncilArea_Melbourne
#          -CouncilArea_Bayside-Suburb_PascoeVale-Suburb_Footscray-Suburb_Glenroy
#          -Suburb_BentleighEast-CouncilArea_Manningham-Suburb_Carnegie-CouncilArea_Monash
#          -Suburb_Brunswick-Suburb_Coburg-Suburb_AscotVale-Suburb_Yarraville
#          -CouncilArea_Brimbank-CouncilArea_Darebin-Suburb_MooneePonds-CouncilArea_MooneeValley
#          -Suburb_Thornbury-CouncilArea_GlenEira,data=RE_train1)
# summary(REfit)


# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter,data=RE_train1)

# summary(REfit)
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL,data=RE_train1)
# 
# summary(REfit)
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle,data=RE_train1)
# 
# summary(REfit)
# 
# 
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Village,data=RE_train1)
# 
# summary(REfit)
# 
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Village
#          -`SellerG_Gunn&Co`,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Village
#          -`SellerG_Gunn&Co`-SellerG_Hunter,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Village
#          -`SellerG_Gunn&Co`-SellerG_Hunter-SellerG_Barry,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Village
#          -`SellerG_Gunn&Co`-SellerG_Hunter-SellerG_Barry-SellerG_Trimson,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Village
#          -`SellerG_Gunn&Co`-SellerG_Hunter-SellerG_Barry-SellerG_Trimson-SellerG_Garvey,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Village
#          -`SellerG_Gunn&Co`-SellerG_Hunter-SellerG_Barry-SellerG_Trimson-SellerG_Garvey
#          -SellerG_New,data=RE_train1)
# 
# summary(REfit)
# 
# 
# REfit=lm(Price~.-CouncilArea_ -Method_S-Postcode-Distance-SellerG_Nelson
#          -CouncilArea_Maribyrnong-CouncilArea_Yarra-SellerG_Weda-SellerG_C21
#          -SellerG_Peter-SellerG_GL-SellerG_Pride-SellerG_William-SellerG_Harcourts
#          -SellerG_Buckingham-SellerG_Walshe-SellerG_ASL-SellerG_Eview-SellerG_W.B.
#          -SellerG_Rendina-SellerG_Brace-SellerG_Morrison-Suburb_Seddon-SellerG_Stockdale
#          -SellerG_Prof.-SellerG_Chambers-SellerG_Holland-SellerG_Burnham-Suburb_Thornbury
#          -SellerG_Jas-Suburb_Brunswick-`SellerG_D'Aprano`-SellerG_Dingle-SellerG_Alexkarbon
#          -SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Village
#          -`SellerG_Gunn&Co`-SellerG_Hunter-SellerG_Barry-SellerG_Trimson-SellerG_Garvey
#          -SellerG_New-SellerG_Re-SellerG_Maddison-SellerG_Buxton-SellerG_Barlow
#          -Suburb_Doncaster-Address_BarklySt-SellerG_Matthew-SellerG_MICM-Suburb_Moorabbin
#          -SellerG_RW-`SellerG_O'Brien`-Address_HighSt-Suburb_Ivanhoe-CouncilArea_Manningham
#          -Suburb_BrunswickEast-Suburb_AscotVale-SellerG_Pagan-CouncilArea_HobsonsBay
#          -SellerG_Darren-SellerG_Hodges-SellerG_Purplebricks-SellerG_Leased
#          -CouncilArea_Moreland-SellerG_Thomas-SellerG_Chisholm-SellerG_Gary
#          -Suburb_Aberfeldie-SellerG_Williams-SellerG_Bells-SellerG_Moonee
#          -SellerG_Harrington-`SellerG_O'Donoghues`-CouncilArea_Monash-SellerG_Edward
#          -CouncilArea_Bayside-SellerG_Whiting-SellerG_Nicholson-SellerG_FN-SellerG_Jason
#          -SellerG_Parkes-SellerG_Frank-Suburb_BentleighEast-SellerG_Christopher
#          -SellerG_Morleys-Suburb_Strathmore-SellerG_Rodney-Suburb_BrunswickWest-Suburb_Oakleigh
#          -Suburb_Ashwood-Suburb_HamptonEast-Suburb_Southbank-Suburb_GlenHuntly
#          -`SellerG_Raine&Horne`-Suburb_SouthMelbourne-SellerG_Wilson-SellerG_Domain
#          -SellerG_Anderson-SellerG_Bayside-SellerG_J-SellerG_Compton-SellerG_First
#          -SellerG_Hamilton-SellerG_HAR-SellerG_LITTLE-SellerG_McDonald-SellerG_hockingstuart
#          -SellerG_Owen-Suburb_Flemington-Suburb_Kensington-SellerG_Beller-Suburb_Chadstone
#          -SellerG_Ray-Suburb_Fairfield,data=RE_train1)
# 
# summary(REfit)
# 
# REfit=step(REfit)
# 
# summary(REfit)
# 
# formula(REfit)
# 
# REfit = lm(Price ~ Rooms + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
#              YearBuilt + Suburb_Gowanbrae + Suburb_Spotswood + Suburb_Yallambie + 
#              Suburb_CaulfieldNorth + Suburb_Parkville + Suburb_Alphington + 
#              Suburb_Hughesdale + Suburb_CarltonNorth + Suburb_Viewbank + 
#              Suburb_MontAlbert + Suburb_Watsonia + Suburb_Heidelberg + 
#              Suburb_CaulfieldSouth + Suburb_Fitzroy + Suburb_Braybrook + 
#              Suburb_Canterbury + Suburb_CliftonHill + Suburb_KewEast + 
#              Suburb_AlbertPark + Suburb_BoxHill + Suburb_Windsor + Suburb_Elsternwick + 
#              Suburb_Collingwood + Suburb_OakPark + Suburb_Altona + Suburb_Hadfield + 
#              Suburb_Abbotsford + Suburb_HeidelbergWest + Suburb_OakleighSouth + 
#              Suburb_CoburgNorth + Suburb_Murrumbeena + Suburb_HeidelbergHeights + 
#              Suburb_Malvern + Suburb_Ashburton + Suburb_Rosanna + Suburb_Niddrie + 
#              Suburb_Maidstone + Suburb_AirportWest + Suburb_FitzroyNorth + 
#              Suburb_Bulleen + Suburb_Ormond + Suburb_SunshineNorth + Suburb_WestFootscray + 
#              Suburb_AvondaleHeights + Suburb_Fawkner + Suburb_AltonaNorth + 
#              Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
#              Suburb_Melbourne + Suburb_SunshineWest + Suburb_TemplestoweLower + 
#              Suburb_KeilorEast + Suburb_HawthornEast + Suburb_Prahran + 
#              Suburb_SurreyHills + Suburb_Toorak + Suburb_Elwood + Suburb_Maribyrnong + 
#              Suburb_Newport + Suburb_MooneePonds + Suburb_Hampton + Suburb_Balwyn + 
#              Suburb_MalvernEast + Suburb_Camberwell + Suburb_Carnegie + 
#              Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_PascoeVale + 
#              Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
#              Suburb_Coburg + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
#              Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + Suburb_SouthYarra + 
#              Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_Reservoir + 
#              Type_u + Type_h + SellerG_Assisi + SellerG_Kelly + SellerG_Bekdon + 
#              SellerG_Castran + SellerG_Caine + SellerG_Lindellas + SellerG_Paul + 
#              `SellerG_Abercromby's` + SellerG_Philip + SellerG_Considine + 
#              SellerG_Thomson + SellerG_Cayzer + SellerG_Collins + SellerG_Raine + 
#              SellerG_Love + SellerG_Douglas + SellerG_Kay + SellerG_McGrath + 
#              SellerG_Noel + SellerG_Miles + SellerG_Greg + SellerG_Sweeney + 
#              SellerG_RT + SellerG_Marshall + SellerG_Jellis + Address_DandenongRd + 
#              Address_ToorakRd + Address_StKildaRd + Method_VB + Method_SP + 
#              Method_PI + CouncilArea_Kingston + CouncilArea_Whitehorse + 
#              CouncilArea_Brimbank + CouncilArea_Melbourne + CouncilArea_Banyule + 
#              CouncilArea_PortPhillip + CouncilArea_Stonnington + CouncilArea_Darebin + 
#              CouncilArea_Boroondara, data=RE_train1)
# 
# summary(REfit)
# 
# REfit = lm(Price ~ Rooms + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
#              YearBuilt + Suburb_Gowanbrae + Suburb_Spotswood + Suburb_Yallambie + 
#              Suburb_CaulfieldNorth + Suburb_Parkville + Suburb_Alphington + 
#              Suburb_Hughesdale + Suburb_CarltonNorth + Suburb_Viewbank + 
#              Suburb_MontAlbert + Suburb_Watsonia + 
#              Suburb_CaulfieldSouth + Suburb_Fitzroy + Suburb_Braybrook + 
#              Suburb_Canterbury + Suburb_CliftonHill + Suburb_KewEast + 
#              Suburb_AlbertPark + Suburb_BoxHill + Suburb_Windsor + Suburb_Elsternwick + 
#              Suburb_Collingwood + Suburb_OakPark + Suburb_Altona + Suburb_Hadfield + 
#              Suburb_Abbotsford + Suburb_HeidelbergWest + Suburb_OakleighSouth + 
#              Suburb_CoburgNorth + Suburb_Murrumbeena + Suburb_HeidelbergHeights + 
#              Suburb_Malvern + Suburb_Ashburton + Suburb_Rosanna + Suburb_Niddrie + 
#              Suburb_Maidstone + Suburb_AirportWest + Suburb_FitzroyNorth + 
#              Suburb_Bulleen + Suburb_Ormond + Suburb_SunshineNorth + Suburb_WestFootscray + 
#              Suburb_AvondaleHeights + Suburb_Fawkner + Suburb_AltonaNorth + 
#              Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
#              Suburb_Melbourne + Suburb_SunshineWest + Suburb_TemplestoweLower + 
#              Suburb_KeilorEast + Suburb_HawthornEast + Suburb_Prahran + 
#              Suburb_SurreyHills + Suburb_Toorak + Suburb_Elwood + Suburb_Maribyrnong + 
#              Suburb_Newport + Suburb_MooneePonds + Suburb_Hampton + Suburb_Balwyn + 
#              Suburb_MalvernEast + Suburb_Camberwell + Suburb_Carnegie + 
#              Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_PascoeVale + 
#              Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
#              Suburb_Coburg + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
#              Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + Suburb_SouthYarra + 
#              Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_Reservoir + 
#              Type_u + Type_h + SellerG_Assisi + SellerG_Kelly + SellerG_Bekdon + 
#              SellerG_Castran + SellerG_Caine + SellerG_Lindellas + SellerG_Paul + 
#              `SellerG_Abercromby's` + SellerG_Philip + SellerG_Considine + 
#              SellerG_Cayzer + SellerG_Collins + SellerG_Raine + 
#              SellerG_Douglas + SellerG_Kay + SellerG_McGrath + 
#              SellerG_Noel + SellerG_Miles + SellerG_Greg + SellerG_Sweeney + 
#              SellerG_RT + SellerG_Marshall + SellerG_Jellis + Address_DandenongRd + 
#              Address_ToorakRd + Address_StKildaRd + Method_VB + Method_SP + 
#              Method_PI + CouncilArea_Kingston + CouncilArea_Whitehorse + 
#              CouncilArea_Brimbank + CouncilArea_Melbourne + CouncilArea_Banyule + 
#              CouncilArea_PortPhillip + CouncilArea_Stonnington + CouncilArea_Darebin + 
#              CouncilArea_Boroondara, data=RE_train1)
# 
# summary(REfit)
# 
# REfit = lm(Price ~ Rooms + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
#              YearBuilt + Suburb_Gowanbrae + Suburb_Spotswood + Suburb_Yallambie + 
#              Suburb_CaulfieldNorth + Suburb_Parkville + 
#              Suburb_CarltonNorth + Suburb_Viewbank + 
#              Suburb_MontAlbert + Suburb_Watsonia + 
#              Suburb_CaulfieldSouth + Suburb_Fitzroy + Suburb_Braybrook + 
#              Suburb_Canterbury + Suburb_CliftonHill + Suburb_KewEast + 
#              Suburb_AlbertPark + Suburb_BoxHill + Suburb_Windsor + Suburb_Elsternwick + 
#              Suburb_Collingwood + Suburb_OakPark + Suburb_Altona + Suburb_Hadfield + 
#              Suburb_Abbotsford + Suburb_HeidelbergWest + Suburb_OakleighSouth + 
#              Suburb_CoburgNorth + Suburb_HeidelbergHeights + 
#              Suburb_Malvern + Suburb_Ashburton + Suburb_Rosanna + Suburb_Niddrie + 
#              Suburb_Maidstone + Suburb_AirportWest + Suburb_FitzroyNorth + 
#              Suburb_Bulleen + Suburb_Ormond + Suburb_SunshineNorth + Suburb_WestFootscray + 
#              Suburb_AvondaleHeights + Suburb_Fawkner + Suburb_AltonaNorth + 
#              Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
#              Suburb_Melbourne + Suburb_SunshineWest + Suburb_TemplestoweLower + 
#              Suburb_KeilorEast + Suburb_HawthornEast + Suburb_Prahran + 
#              Suburb_SurreyHills + Suburb_Toorak + Suburb_Elwood + Suburb_Maribyrnong + 
#              Suburb_Newport + Suburb_MooneePonds + Suburb_Hampton + Suburb_Balwyn + 
#              Suburb_MalvernEast + Suburb_Camberwell + Suburb_Carnegie + 
#              Suburb_Bentleigh + Suburb_PascoeVale + 
#              Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
#              Suburb_Coburg + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
#              Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + Suburb_SouthYarra + 
#              Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_Reservoir + 
#              Type_u + Type_h + SellerG_Assisi + SellerG_Kelly + SellerG_Bekdon + 
#              SellerG_Castran + SellerG_Caine + SellerG_Lindellas + SellerG_Paul + 
#              `SellerG_Abercromby's` + SellerG_Philip + SellerG_Considine + 
#              SellerG_Cayzer + SellerG_Collins + 
#              SellerG_Douglas + SellerG_Kay  + 
#              SellerG_Noel + SellerG_Miles + SellerG_Greg + SellerG_Sweeney + 
#              SellerG_RT + SellerG_Marshall + SellerG_Jellis + Address_DandenongRd + 
#              Address_ToorakRd + Address_StKildaRd + Method_VB + Method_SP + 
#              Method_PI + CouncilArea_Kingston + CouncilArea_Whitehorse + 
#              CouncilArea_Brimbank + CouncilArea_Melbourne + 
#              CouncilArea_PortPhillip + CouncilArea_Stonnington + CouncilArea_Darebin, data=RE_train1)
# 
# summary(REfit)

val.pred=predict(REfit,newdata=RE_train2)

errors=RE_train2$Price-val.pred

RMSE = errors**2 %>% mean() %>% sqrt()
RMSE
## 407887.8
##  score = 212467/407887.8 = 0.52089
REfit.final=REfit=lm(Price ~ .,
                 data=RE_train)


summary(REfit.final)
REfit.final=step(REfit.final)

summary(REfit.final)

formula(REfit.final)

REfit.final = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                   Landsize + BuildingArea + YearBuilt + Suburb_CouncilArea_Melbourne_Melbourne + 
                   Suburb_CouncilArea_Balwyn_ + Suburb_CouncilArea_CliftonHill_Yarra + 
                   Suburb_CouncilArea_Canterbury_Boroondara + Suburb_CouncilArea_Preston_ + 
                   Suburb_CouncilArea_SurreyHills_Whitehorse + Suburb_CouncilArea_BrightonEast_ + 
                   Suburb_CouncilArea_Camberwell_ + Suburb_CouncilArea_GlenIris_Stonnington + 
                   Suburb_CouncilArea_Abbotsford_Yarra + Suburb_CouncilArea_Ashburton_Boroondara + 
                   Suburb_CouncilArea_CoburgNorth_Moreland + Suburb_CouncilArea_Kew_ + 
                   Suburb_CouncilArea_BoxHill_Whitehorse + Suburb_CouncilArea_Elsternwick_GlenEira + 
                   Suburb_CouncilArea_Collingwood_Yarra + Suburb_CouncilArea_Hadfield_Moreland + 
                   Suburb_CouncilArea_Altona_HobsonsBay + Suburb_CouncilArea_Malvern_Stonnington + 
                   Suburb_CouncilArea_Melbourne_ + Suburb_CouncilArea_SurreyHills_Boroondara + 
                   Suburb_CouncilArea_HeidelbergHeights_Banyule + Suburb_CouncilArea_HeidelbergWest_Banyule + 
                   Suburb_CouncilArea_Niddrie_MooneeValley + Suburb_CouncilArea_StKilda_ + 
                   Suburb_CouncilArea_AvondaleHeights_MooneeValley + Suburb_CouncilArea_BrunswickEast_Moreland + 
                   Suburb_CouncilArea_Moorabbin_Kingston + Suburb_CouncilArea_SouthMelbourne_PortPhillip + 
                   Suburb_CouncilArea_SunshineWest_Brimbank + Suburb_CouncilArea_Armadale_Stonnington + 
                   Suburb_CouncilArea_Fawkner_Moreland + Suburb_CouncilArea_Glenroy_ + 
                   Suburb_CouncilArea_WestFootscray_Maribyrnong + Suburb_CouncilArea_Kensington_Melbourne + 
                   Suburb_CouncilArea_Williamstown_HobsonsBay + Suburb_CouncilArea_BrunswickWest_Moreland + 
                   Suburb_CouncilArea_Toorak_Stonnington + Suburb_CouncilArea_Maribyrnong_Maribyrnong + 
                   Suburb_CouncilArea_Prahran_Stonnington + Suburb_CouncilArea_Doncaster_Manningham + 
                   Suburb_CouncilArea_Balwyn_Boroondara + Suburb_CouncilArea_HawthornEast_Boroondara + 
                   Suburb_CouncilArea_Camberwell_Boroondara + Suburb_CouncilArea_PortMelbourne_PortPhillip + 
                   Suburb_CouncilArea_Elwood_PortPhillip + Suburb_CouncilArea_Hampton_Bayside + 
                   Suburb_CouncilArea_MalvernEast_Stonnington + Suburb_CouncilArea_AscotVale_MooneeValley + 
                   Suburb_CouncilArea_GlenIris_Boroondara + Suburb_CouncilArea_Glenroy_Moreland + 
                   Suburb_CouncilArea_Bentleigh_GlenEira + Suburb_CouncilArea_PascoeVale_Moreland + 
                   Suburb_CouncilArea_BalwynNorth_Boroondara + Suburb_CouncilArea_Kew_Boroondara + 
                   Suburb_CouncilArea_Reservoir_ + Suburb_CouncilArea_SouthYarra_Stonnington + 
                   Suburb_CouncilArea_StKilda_PortPhillip + Suburb_CouncilArea_Brighton_Bayside + 
                   Suburb_CouncilArea_Northcote_Darebin + Suburb_CouncilArea_Coburg_Moreland + 
                   Suburb_CouncilArea_Essendon_MooneeValley + Suburb_CouncilArea_Brunswick_Moreland + 
                   Suburb_CouncilArea_Preston_Darebin + Suburb_CouncilArea_BentleighEast_GlenEira + 
                   Suburb_CouncilArea_Richmond_Yarra + Suburb_CouncilArea_Reservoir_Darebin + 
                   Type_u + Type_h + Method_VB + Method_SP + Method_PI,data=RE_train)

summary(REfit.final)

# REfit.final = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
#                    Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + 
#                    Suburb_AscotVale + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + 
#                    Suburb_MalvernEast + Suburb_Camberwell + Suburb_PortMelbourne + 
#                    Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
#                    Suburb_Coburg + Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + 
#                    Suburb_Essendon + Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + 
#                    Suburb_Richmond + Suburb_Reservoir + Type_u + Type_h + Method_VB + 
#                    Method_SP + Method_PI + CouncilArea_Monash + CouncilArea_Whitehorse + 
#                    CouncilArea_Manningham + CouncilArea_Brimbank + CouncilArea_HobsonsBay + 
#                    CouncilArea_Melbourne + CouncilArea_Banyule + 
#                    CouncilArea_Yarra + CouncilArea_Maribyrnong + 
#                    CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_Darebin + 
#                    CouncilArea_MooneeValley + CouncilArea_Boroondara + 
#                    CouncilArea_,data=RE_train)


REfit.final = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                         Landsize + BuildingArea + YearBuilt + Suburb_CouncilArea_Melbourne_Melbourne + 
                         Suburb_CouncilArea_Balwyn_ + 
                         Suburb_CouncilArea_Canterbury_Boroondara + Suburb_CouncilArea_Preston_ + 
                         Suburb_CouncilArea_SurreyHills_Whitehorse + Suburb_CouncilArea_BrightonEast_ + 
                         Suburb_CouncilArea_Camberwell_ + Suburb_CouncilArea_GlenIris_Stonnington + 
                         Suburb_CouncilArea_Abbotsford_Yarra + Suburb_CouncilArea_Ashburton_Boroondara + 
                         Suburb_CouncilArea_CoburgNorth_Moreland + Suburb_CouncilArea_Kew_ + 
                         Suburb_CouncilArea_BoxHill_Whitehorse + 
                         Suburb_CouncilArea_Collingwood_Yarra + Suburb_CouncilArea_Hadfield_Moreland + 
                         Suburb_CouncilArea_Altona_HobsonsBay + Suburb_CouncilArea_Malvern_Stonnington + 
                         Suburb_CouncilArea_Melbourne_ + Suburb_CouncilArea_SurreyHills_Boroondara + 
                         Suburb_CouncilArea_HeidelbergHeights_Banyule + Suburb_CouncilArea_HeidelbergWest_Banyule + 
                         Suburb_CouncilArea_Niddrie_MooneeValley + Suburb_CouncilArea_StKilda_ + 
                         Suburb_CouncilArea_AvondaleHeights_MooneeValley + Suburb_CouncilArea_BrunswickEast_Moreland + 
                         Suburb_CouncilArea_Moorabbin_Kingston + Suburb_CouncilArea_SouthMelbourne_PortPhillip + 
                         Suburb_CouncilArea_SunshineWest_Brimbank + Suburb_CouncilArea_Armadale_Stonnington + 
                         Suburb_CouncilArea_Fawkner_Moreland + Suburb_CouncilArea_Glenroy_ + 
                         Suburb_CouncilArea_WestFootscray_Maribyrnong + 
                         Suburb_CouncilArea_Williamstown_HobsonsBay + Suburb_CouncilArea_BrunswickWest_Moreland + 
                         Suburb_CouncilArea_Toorak_Stonnington + Suburb_CouncilArea_Maribyrnong_Maribyrnong + 
                         Suburb_CouncilArea_Prahran_Stonnington + Suburb_CouncilArea_Doncaster_Manningham + 
                         Suburb_CouncilArea_Balwyn_Boroondara + Suburb_CouncilArea_HawthornEast_Boroondara + 
                         Suburb_CouncilArea_Camberwell_Boroondara + Suburb_CouncilArea_PortMelbourne_PortPhillip + 
                         Suburb_CouncilArea_Elwood_PortPhillip + Suburb_CouncilArea_Hampton_Bayside + 
                         Suburb_CouncilArea_MalvernEast_Stonnington +  
                         Suburb_CouncilArea_GlenIris_Boroondara + Suburb_CouncilArea_Glenroy_Moreland + 
                         Suburb_CouncilArea_Bentleigh_GlenEira + Suburb_CouncilArea_PascoeVale_Moreland + 
                         Suburb_CouncilArea_BalwynNorth_Boroondara + Suburb_CouncilArea_Kew_Boroondara + 
                         Suburb_CouncilArea_Reservoir_ + Suburb_CouncilArea_SouthYarra_Stonnington + 
                         Suburb_CouncilArea_StKilda_PortPhillip + Suburb_CouncilArea_Brighton_Bayside + 
                         Suburb_CouncilArea_Northcote_Darebin + Suburb_CouncilArea_Coburg_Moreland + 
                         Suburb_CouncilArea_Essendon_MooneeValley + Suburb_CouncilArea_Brunswick_Moreland + 
                         Suburb_CouncilArea_Preston_Darebin + 
                         Suburb_CouncilArea_Richmond_Yarra + Suburb_CouncilArea_Reservoir_Darebin + 
                         Type_u + Type_h + Method_VB + Method_SP + Method_PI,data=RE_train)


summary(REfit.final)
test.pred=predict(REfit.final,newdata=RE_test)

write.table(test.pred,"padmaja_Saripalli_P1_part2new.csv",col.names = c("Price"),row.names = F)

plot(REfit.final,1) 

plot(REfit.final,2) 
plot(REfit.final,3) 
plot(REfit.final,4) 



####---------------------    DecisionTree Regression   ------------------#######

library(tree)
REfit.tree=tree(Price~.,data=RE_train1)

REfit.tree

plot(REfit.tree)
text(REfit.tree)

val.Price=predict(REfit.tree,newdata = RE_train2)

rmse_val=((val.Price)-(RE_train2$Price))^2 %>% mean() %>% sqrt()

rmse_val
## 419848.9
## score = 212467/419848.9=0.50605

REfit.tree.final=tree(Price~.,data=RE_train)
test.pred_tree=predict(REfit.tree.final,newdata=RE_test)
write.csv(test.pred_tree,"padmaja_Saripalli_P1_part2_tree.csv",row.names = F)



####------------------------  RandomForest Regression--------------#####

library(randomForest)
library(cvTools)

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

num_trials=50
my_params=subset_paras(param,num_trials)
my_params

glimpse(RE_train)

myerror=9999999

for(i in 1:num_trials){
   print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  k=cvTuning(randomForest,Price~.,
             data =RE_train,
             tuning =params,
             folds = cvFolds(nrow(RE_train), K=10, type = "random"),
             seed =21
  )
  score.this=k$cv[,2]
  if(score.this<myerror){
    
     print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
     print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
   print('DONE')
  # uncomment the line above to keep track of progress
}

myerror
## 393374.9
## score = 212467/393374.9 =0.5401
best_params

REfit.rf.final=randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=RE_train)

test.pred.rf=predict(REfit.rf.final,newdata = RE_test)
write.table(test.pred.rf,"padmaja_Saripalli_P1_part2_rf.csv",col.names = c("Price"),row.names = F)
