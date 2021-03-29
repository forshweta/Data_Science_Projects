
## Loading the Required libraries

library(ggplot2) # Library for Plots
library(moments) # Library for functions like skew, kurtosis etc

library(readxl)  # Library to read excel file

setwd ("/Users/shweta/Desktop/PGPBABI/DATA MINING/project")
getwd()
install

##Importing the data

tbank <- read_excel("Thera Bank_dataset copy.xlsx")

##Viewing the data 
View(tbank)

##Viewing the structure of the data (data types)
str(tbank)
head(tbank)

##Change the variable names in R supported format
colnames(tbank) = make.names(colnames(tbank))

##Summary of the data
nrow(tbank)
dim(tbank)
#There are 5000 rows in all and 14 variables

##Checking for missing values if any:
any(is.na(tbank))
sum(is.na(tbank))
apply(tbank,2, function(x) sum(is.na(x)))

#only family members has 18 missing values

## Treatment for missing values

library(mice)
md.pattern(tbank)
impute = mice(tbank,m =5,method ="pmm", seed = 200 )
tbank_wo_na = complete(impute,3)
any(is.na(tbank_wo_na))


##Checking our dependent variable - Personal Loan
colnames(tbank_wo_na)
prop.table(table(tbank_wo_na$Personal.Loan))
hist(tbank_wo_na$Personal.Loan)

# only 480 or 9.6% of customers have availed the personal loan out of total 5000 customers

### Before starting anything on the original data set file , make a copy of the same

tbank2 = tbank_wo_na

## Convert the datatypes of catagorical variables
tbank2$Education = as.factor(tbank2$Education)
tbank2$Personal.Loan = as.factor(tbank2$Personal.Loan)
tbank2$Securities.Account = as.factor(tbank2$Securities.Account)
tbank2$CD.Account = as.factor(tbank2$CD.Account)
tbank2$Online = as.factor(tbank2$Online)
tbank2$ZIP.Code = as.factor(tbank2$ZIP.Code)
tbank2$CreditCard = as.factor(tbank2$CreditCard)
summary(tbank2)
str(tbank2)


## analysing each variable
attach(tbank2)
colnames(tbank2)
boxplot(tbank2[,-c(1,5,8,10,11,12,13,14)])
by(tbank2,INDICES = Personal.Loan, FUN = summary)


#### Age-- Integer Variable #####

summary(Age..in.years.)

ggplot(data= tbank2,aes(x=Age..in.years.)) + geom_bar(fill="red",color="black") + ggtitle("Age Distribution")

ggplot(data= tbank2,aes(x=Age..in.years.)) + geom_bar(alpha=0.5,fill="red",color="black") + ggtitle("Age Distribution")

ggplot(data=tbank2, mapping=aes(x="", y=Age..in.years.)) + geom_boxplot(aes(color= Personal.Loan))

boxplot(Age..in.years.~Personal.Loan,col = c("blue","red"), main = "Loan versus Age Distribtuion")


#### Experience-- Continuous Variable #####



summary(tbank2$Experience..in.years.)


View(Experience..in.years.[Experience..in.years.<0])

sum(tbank2$Experience..in.years.<0)

# negative experience in years  - total 52 values
# We replace the negartive values with imputed values using KNN
tbank2$Experience = tbank2$Experience..in.years.

tbank2$Experience[tbank2$Experience<0] = NA
sum(is.na(tbank2$Experience))
library(DMwR)
tbank3 = knnImputation(tbank2,k=5)
sum(is.na(tbank3$Experience))

attach(tbank3)

ggplot(data= tbank3,aes(x= Experience)) + geom_bar(alpha = 0.25, fill="red",color="black") + ggtitle("Experience Distribution")


ggplot(data=tbank3, mapping=aes(x="", y= Experience)) + geom_boxplot(aes(color= Personal.Loan))

boxplot(Experience~Personal.Loan,col = c("blue","red"), main = "Personl Loan versus Experience Distribtuion")

colnames(tbank3)
tbank3$experience.in.years.2 = NULL
tbank3$experience = NULL
tbank3$Experience..in.years. = NULL
summary(tbank3$Experience)
#### Income -- Continuous Variable #####

summary(tbank3$Income..in.K.year.)
boxplot(tbank3$Income..in.K.year.)
## Identfy outliers and treat them
quantile(Income..in.K.year.)
quantile(Income..in.K.year. , 0.95)
quantile(Income..in.K.year. , 0.05)
iqr = IQR(Income..in.K.year.)
lower.limit = 1.5*iqr - 39
upper.limit =  98 + 1.5*iqr
print(lower.limit)
print(upper.limit)

# we look at the percentile distribution and 
# cap the extremes to acceptable bounds, 
#cap all values below the 5th percentile to the value at 5th and all values above 95th percentile to the value of 95th percentile.

##upper extreme values
nrow(tbank3[tbank3$Income..in.K.year.>170,]) ## no of outliers is 249

##lower extreme values
nrow(tbank3[tbank3$Income..in.K.year.<18,]) ## no of outliers is 225

tbank3$Income..in.K.year.[tbank3$Income..in.K.year.>170] = 170
tbank3$Income..in.K.year.[tbank3$Income..in.K.year.<18] = 18

##lets try to see the observations with outliers

tbanknew = tbank[tbank$Income..in.K.year.>170,]
head(tbanknew)

summary(tbank3$Income..in.K.year.)


ggplot(data= tbank3,aes(x= Income..in.K.year.)) + geom_bar(alpha = 0.25, fill="red",color="black") + ggtitle("Income Distribution")


ggplot(data=tbank3, mapping=aes(x="", y= Income..in.K.year.)) + geom_boxplot(aes(color= Personal.Loan))

boxplot(Income..in.K.year.~Personal.Loan,col = c("blue","red"),data = tbank3, main = "Personl Loan versus Income Distribtuion")



#### Zip Code -- Categorical Variable #####

summary(ZIP.Code)
table(ZIP.Code)

table(ZIP.Code, Personal.Loan)
prop.table(table(ZIP.Code, Personal.Loan),margin = 2)

#### Family members -- Integer Variable #####

attach(tbank3)
summary(Family.members)

boxplot(Family.members)

ggplot(data= tbank3,aes(x= Family.members)) + geom_bar(alpha = 0.25, fill="red",color="black") + ggtitle("Family members Distribution")

boxplot(Family.members ~ Personal.Loan,col = c("blue","red"), main = "Personl Loan versus Family Members")


table(Family.members, Personal.Loan)

#### CC AVG -- Continuos Variable #####

attach(tbank3)

summary(tbank3$CCAvg)

boxplot(CCAvg)

## Identfy outliers and treat them
quantile(CCAvg)
quantile(CCAvg , 0.95)
quantile(CCAvg , 0.05)

## we look at the percentile distribution and 
## cap the extremes to acceptable bounds, 
##cap all values below the 5th percentile to the value at 5th and all values above 95th percentile to the value of 95th percentile.

##upper extreme values
nrow(tbank3[tbank3$CCAvg>6,]) ## no of outliers is 241

##lower extreme values
nrow(tbank3[tbank3$CCAvg<0.1,]) ## no of outliers is 106


tbank3$CCAvg[tbank3$CCAvg>6] = 6
tbank3$CCAvg[tbank3$CCAvg<0.1] = 0.1

ggplot(data= tbank3,aes(x= CCAvg)) + geom_bar(alpha = 0.25, fill="red",color="black") + ggtitle("CC Avg Distribution")


boxplot(CCAvg ~ Personal.Loan,col = c("blue","red"), main = "Personl Loan versus CCAvg")

## Education - Categorical Variable ###

summary(Education)
#TEducation factor varies from 1 to 3, mostly being on the lower side.
prop.table(table(Education , Personal.Loan),margin = 1)
prop.table(table(Education , Personal.Loan))

## Education - Categorical Variable ###

summary(Education)
#Education factor varies from 1 to 3, mostly being on the lower side.
prop.table(table(Education , Personal.Loan),margin = 1)

colnames(tbank3)

prop.table(table(Education , Personal.Loan))

#### Mortgage -- Integer Variable #####

summary(tbank3$Mortgage)

boxplot(tbank3$Mortgage)

## Identfy outliers and treat them
quantile(Mortgage)
quantile(Mortgage , 0.95)
quantile(Mortgage , 0.05)
summary(tbanknew)
summary(tbank)
## we look at the percentile distribution and 
## cap the extremes to acceptable bounds, 
##cap all values below the 5th percentile to the value at 5th and all values above 95th percentile to the value of 95th percentile.

##upper extreme values
nrow(tbank3[tbank3$Mortgage>272,]) ## no of outliers is 247


tbank3$Mortgage[tbank3$Mortgage>272] = 272

summary(tbank3$Mortgage)

boxplot(tbank3$Mortgage)

ggplot(data= tbank3,aes(x= Mortgage)) + geom_bar(alpha = 0.25, fill="red",color="black") + ggtitle("Mortgage Distribution")

boxplot(Mortgage ~ Personal.Loan,data =tbank3,col = c("blue","red"), main = "Personl Loan versus Mortgage")


##Securities.Account - Categorical Variable ###

summary(Securities.Account)

prop.table(table(Securities.Account , Personal.Loan),margin = 1)

prop.table(table(Securities.Account , Personal.Loan))

##CD.Account - Categorical Variable ###

summary(CD.Account)

prop.table(table(CD.Account , Personal.Loan),margin = 1)
prop.table(table(CD.Account, Personal.Loan))

##Online - Categorical Variable ###

summary(Online)

prop.table(table(Online , Personal.Loan),margin = 1)
prop.table(table(Online, Personal.Loan))

##CreditCard - Categorical Variable ###

summary(CreditCard)

prop.table(table(CreditCard , Personal.Loan),margin = 1)
prop.table(table(CreditCard, Personal.Loan))

attach(tbank3)
colnames(tbank3)
## Chi square test to see significance of each categorical variable
chisq.test(Personal.Loan,ZIP.Code)
chisq.test(Personal.Loan,Family.members)
chisq.test(Personal.Loan,Education)
chisq.test(Personal.Loan,Securities.Account)
chisq.test(Personal.Loan,Education)
chisq.test(Personal.Loan,CreditCard)
chisq.test(Personal.Loan,Online)          
chisq.test(Personal.Loan,CD.Account)             
colnames(tbank2)        
           
##  checking  categorical variable categories with less than 5% of the total observations.
as.matrix((prop.table(table(ZIP.Code)))*100)           
as.matrix((prop.table(table(Education)))*100)             
as.matrix((prop.table(table(Securities.Account)))*100)   
as.matrix((prop.table(table(Family.members)))*100) 
as.matrix((prop.table(table(CD.Account)))*100)   
as.matrix((prop.table(table(Online)))*100)        
as.matrix((prop.table(table(CreditCard)))*100)                    
summary(tbank3$Family.members)
tbank$Family.members =as.numeric(tbank$Family.members)
summary(tbank$Family.members)
### Create a correlation matrix only for numeric columns in the dataset
library(dplyr)   
tbankcor = tbank3%>%select_if(is.numeric)
tbankcor =tbankcor[,-1]
str(tbankcor)                    
names(tbank3)                    
          
#Correlation analysis (Corrplot)
library(corrplot)
datamatrix = cor(tbankcor)
corrplot(datamatrix, method ="number")

## High correlation with some of the features like Age and Experience


####
######
#### Data Preparation and Split for Test and Train #######
####
######

tbankfinal = tbank3[,-1]
tbankfinalWOL = tbank2[,-1]

library(caTools)
set.seed(144) #Input any random number
split = sample.split(tbankfinal$Personal.Loan, SplitRatio = 0.7)
tbankfinal_train= subset(tbankfinal, split == T)
tbankfinal_test= subset(tbankfinal, split == F)
c(nrow(tbankfinal_train),nrow(tbankfinal_test))

### Calculate % distribution
prop.table(table(tbankfinal_train$Personal.Loan))
prop.table(table(tbankfinal_test$Personal.Loan))

##Viewing the Train sample

View(tbankfinal_train)
str(tbankfinal_train)
dim(tbankfinal_train)


###
## Packages for CART Model
###


library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ROCR)
library(ineq)

attach(tbankfinal_train)
##Setting the control parameters for rpart
colnames(tbankfinal_train)

r.ctrl = rpart.control(minsplit=90, minbucket = 30, cp = 0, xval = 10)
#Using rpart to build the tree
tbank.ct <- rpart(formula = Personal.Loan ~ ., data = tbankfinal_train[,-c(3,13,9,11,12)], method = "class", control = r.ctrl)
tbank.ct <- rpart(formula = Personal.Loan ~ ., data = tbankfinal_train, method = "class", control = r.ctrl)
print(tbank.ct)

plot(tbank.ct)

fancyRpartPlot(tbank.ct)
prp(tbank.ct)



##To see how the tree performs
printcp(tbank.ct)
plotcp(tbank.ct)


##Pruning the tree - Prune wherever xerror is min and it starts increasing after that
tbank.ct1<- prune(tbank.ct, cp= .03 ,"CP")
printcp(tbank.ct1)
fancyRpartPlot(tbank.ct1, uniform=TRUE,  main="Pruned Classification Tree")
prp(tbank.ct1, uniform=TRUE,  main="Pruned Classification Tree")
tbank.ct1

plotcp(tbank.ct1)

##Scoring
tbankfinal_train$prediction<- predict(tbank.ct1, tbankfinal_train, type="class")
tbankfinal_train$score <- predict(tbank.ct1, tbankfinal_train,type = "prob")[,"1"]
head(tbankfinal_train)

##Model performance on train dataset

nrow(tbankfinal_train)

cm_train = table( tbankfinal_train$prediction,tbankfinal_train$Personal.Loan)
print(cm_train)

sensitivity_train = cm_train[2,2]/(cm_train[2,2] + cm_train[1,2])
specificity_train = cm_train[1,1]/(cm_train[1,1] + cm_train[2,1])
accuracty_train = ((cm_train[1,1]+cm_train[2,2])/3500)
Error_Train = 1-accuracty_train 

## deciling
probs = seq(0,1,length = 11)
qs = quantile(tbankfinal_train$score,probs)
print(qs)
tbankfinal_train$deciles= cut(tbankfinal_train$score,unique(qs),include.lowest = TRUE)
print(tbankfinal_train$deciles)
head(tbankfinal_train)

library(data.table)
traincart = data.table(tbankfinal_train)
ranktbl = traincart[,list(cnt = length(Personal.Loan),cnt_Loan_1 = sum(Personal.Loan == 1),cnt_Loan_0 = sum(Personal.Loan == 0)), by= deciles][order(-deciles)]
print(ranktbl)
ranktbl$rrate = round(ranktbl$cnt_Loan_1/ranktbl$cnt,4)*100
ranktbl$cim_resp = cumsum(ranktbl$cnt_Loan_1)
ranktbl$cim_non_resp = cumsum(ranktbl$cnt_Loan_0)
ranktbl$cim_rel_resp = round(ranktbl$cim_resp/sum(ranktbl$cnt_Loan_1),4)*100
ranktbl$cim_rel_non_resp = round(ranktbl$cim_non_resp/sum(ranktbl$cnt_Loan_0),4)*100
ranktbl$KS = abs(ranktbl$cim_rel_resp - ranktbl$cim_rel_non_resp)
predobj = prediction(tbankfinal_train$score,tbankfinal_train$Personal.Loan)  
perf = performance(predobj,"tpr","fpr")
plot(perf)
ks = max(perf@y.values[[1]] - perf@x.values[[1]])  
print(ks)  
  
auc = performance(predobj,"auc")
auc = as.numeric(auc@y.values)

print(auc)  

gini = ineq(tbankfinal_train$score,"gini")
print(gini)  


Concordance_train_cart = Concordance(actuals = tbankfinal_train$Personal.Loan,predictedScores = tbankfinal_train$score)  

train_cart_key = c(auc,ks,sensitivity_train,specificity_train,accuracty_train, Error_Train, gini, Concordance_train_cart ) 


##Scoring on Test Dataset

tbankfinal_test$prediction<- predict(tbank.ct1, newdata = tbankfinal_test, type="class")
tbankfinal_test$score <- predict(tbank.ct1, newdata = tbankfinal_test,type = "prob")[,"1"]
head(tbankfinal_test)

##Model performance on test dataset

nrow(tbankfinal_test)

cm_test = table( tbankfinal_test$prediction,tbankfinal_test$Personal.Loan)
print(cm_test)

sensitivity_test = cm_test[2,2]/(cm_test[2,2] + cm_test[1,2])
specificity_test = cm_test[1,1]/(cm_test[1,1] + cm_test[2,1])
accuracty_test = ((cm_test[1,1]+cm_test[2,2])/1500)
Error_Test = 1-accuracty_test 


## deciling
probs = seq(0,1,length = 11)
qs_test = quantile(tbankfinal_test$score,probs)
print(qs_test)
tbankfinal_test$deciles= cut(tbankfinal_test$score,unique(qs_test),include.lowest = TRUE)
print(tbankfinal_test$deciles)
head(tbankfinal_test)

library(data.table)
testcart = data.table(tbankfinal_test)
ranktbl_test = testcart[,list(cnt = length(Personal.Loan),cnt_Loan_1 = sum(Personal.Loan == 1),cnt_Loan_0 = sum(Personal.Loan == 0)), by= deciles][order(-deciles)]
print(ranktbl_test)
ranktbl_test$rrate_test = round(ranktbl_test$cnt_Loan_1/ranktbl_test$cnt,4)*100
ranktbl_test$cim_resp_test = cumsum(ranktbl_test$cnt_Loan_1)
ranktbl_test$cim_non_resp_test = cumsum(ranktbl_test$cnt_Loan_0)
ranktbl_test$cim_rel_resp_test = round(ranktbl_test$cim_resp_test/sum(ranktbl_test$cnt_Loan_1),4)*100
ranktbl_test$cim_rel_non_resp_test = round(ranktbl_test$cim_non_resp_test/sum(ranktbl_test$cnt_Loan_0),4)*100
ranktbl_test$KS_test = abs(ranktbl_test$cim_rel_resp_test - ranktbl_test$cim_rel_non_resp_test)
predobj_test = prediction(tbankfinal_test$score,tbankfinal_test$Personal.Loan)  
perf_test = performance(predobj_test,"tpr","fpr")
plot(perf_test)
ks_test = max(perf@y.values[[1]] - perf@x.values[[1]])  
print(ks_test)  

auc_test = performance(predobj_test,"auc")
auc_test = as.numeric(auc_test@y.values)

print(auc_test)  

gini_test = ineq(tbankfinal_test$score,"gini")
print(gini_test)  

library("InformationValue")  
Concordance_test_cart = Concordance(actuals = tbankfinal_test$Personal.Loan,predictedScores = tbankfinal_test$score)  

test_cart_key = c(auc_test,ks_test,sensitivity_test,specificity_test,accuracty_test, Error_Test, gini_test, Concordance_test_cart ) 

### CHAID ###
### As this algorithm works only with the categorical columns and hence only factor columns were selected
library(dplyr)

tbank_train_chaid = tbankfinal_train %>% 
  select_if(is.factor)
tbank_test_chaid = tbankfinal_test%>% select_if(is.factor)

tbank_train_chaid = tbank_train_chaid %>% 
  select(-prediction,-deciles)
tbank_test_chaid = tbank_test_chaid %>% 
  select(-prediction,-deciles)

str(tbank_train_chaid)
str(tbank_test_chaid) 

install.packages("partykit")
install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source")
library(CHAID)

### Merging Threshold: merge sub-categories in predictor variable 
## if p-value is above alpha2 threshold

### Splitting Threshold: If p-value below alpha4 consider the predictor for splitting of the node

ctrl <- chaid_control(minbucket = 30, minsplit = 90, alpha2=.05, alpha4 = .05)
names(tbank_train_chaid)
attach(tbank_train_chaid)

chaid.tree <-chaid(Personal.Loan ~., data=tbank_train_chaid[,-1], control = ctrl)
print(chaid.tree)
plot(chaid.tree)

plot(chaid.tree, type = "simple")
tbank_train_chaid$Prediction_chaid = predict(chaid.tree, newdata = tbank_train_chaid)
tbank_train_chaid$Score_chaid = predict(chaid.tree, newdata = tbank_train_chaid,type = "prob")[,"1"]
head(tbank_train_chaid)
##Chaid Model performance on train dataset

nrow(tbank_train_chaid)

cm_train_chaid = table( tbank_train_chaid$Prediction_chaid,tbank_train_chaid$Personal.Loan)
print(cm_train_chaid)

sensitivity_train_chaid = cm_train_chaid[2,2]/(cm_train_chaid[2,2] + cm_train_chaid[1,2])
specificity_train_chaid = cm_train_chaid[1,1]/(cm_train_chaid[1,1] + cm_train_chaid[2,1])
accuracy_train_chaid = ((cm_train_chaid[1,1]+cm_train_chaid[2,2])/3500)
Error_Train_chaid = 1-accuracy_train_chaid

## deciling
probs = seq(0,1,length = 11)
qs_train_chaid = quantile(tbank_train_chaid$Score_chaid,probs)
print(qs_train_chaid)
tbank_train_chaid$deciles= cut(tbank_train_chaid$Score_chaid,unique(qs_train_chaid),include.lowest = TRUE)
print(tbank_train_chaid$deciles)
head(tbank_train_chaid)

library(data.table)
trainchaid = data.table(tbank_train_chaid)
ranktbl_train_chaid = trainchaid[,list(cnt = length(Personal.Loan),cnt_Loan_1 = sum(Personal.Loan == 1),cnt_Loan_0 = sum(Personal.Loan == 0)), by= deciles][order(-deciles)]
print(ranktbl_train_chaid)
ranktbl_train_chaid$rrate = round(ranktbl_train_chaid$cnt_Loan_1/ranktbl_train_chaid$cnt,4)*100
ranktbl_train_chaid$cim_resp = cumsum(ranktbl_train_chaid$cnt_Loan_1)
ranktbl_train_chaid$cim_non_resp = cumsum(ranktbl_train_chaid$cnt_Loan_0)
ranktbl_train_chaid$cim_rel_resp = round(ranktbl_train_chaid$cim_resp/sum(ranktbl_train_chaid$cnt_Loan_1),4)*100
ranktbl_train_chaid$cim_rel_non_resp = round(ranktbl_train_chaid$cim_non_resp/sum(ranktbl_train_chaid$cnt_Loan_0),4)*100
ranktbl_train_chaid$KS = abs(ranktbl_train_chaid$cim_rel_resp - ranktbl_train_chaid$cim_rel_non_resp)
predobj_chaid_train = prediction(tbank_train_chaid$Score_chaid,tbank_train_chaid$Personal.Loan)  
perf_train_chaid = performance(predobj_chaid_train,"tpr","fpr")
plot(perf_train_chaid)
ks_chaid_train = max(perf_train_chaid@y.values[[1]] - perf_train_chaid@x.values[[1]])  
print(ks_chaid_train)  

auc_train_chaid = performance(predobj_chaid_train,"auc")
auc_train_chaid  = as.numeric(auc_train_chaid @y.values)

print(auc_train_chaid )  

gini_train_chaid = ineq(tbank_train_chaid$Score_chaid,"gini")
print(gini_train_chaid )  


library("InformationValue")  
Concordance(actuals = tbank_train_chaid$Personal.Loan,predictedScores = tbank_train_chaid$Score_chaid)  


Concordance_train_chaid = Concordance(actuals = tbank_train_chaid$Personal.Loan,predictedScores = tbank_train_chaid$Score_chaid) 
train_chaid_key = c(auc_train_chaid,ks_chaid_train,sensitivity_train_chaid,specificity_train_chaid,accuracy_train_chaid, Error_Train_chaid, gini_train_chaid, Concordance_train_chaid) 

##Scoring on Chaid Test Dataset

tbank_test_chaid$prediction_chaid_test<- predict(chaid.tree, newdata = tbank_test_chaid)
tbank_test_chaid$score_chaid_test <- predict(chaid.tree, newdata = tbank_test_chaid,type = "prob")[,"1"]
head(tbank_test_chaid)

##Model performance on test dataset

nrow(tbank_test_chaid)

cm_test_chaid = table( tbank_test_chaid$prediction_chaid_test,tbank_test_chaid$Personal.Loan)
print(cm_test_chaid)

sensitivity_test_chaid = cm_test_chaid [2,2]/(cm_test_chaid [2,2] + cm_test_chaid [1,2])
specificity_test_chaid = cm_test_chaid [1,1]/(cm_test_chaid[1,1] + cm_test_chaid [2,1])
accuracty_test_chaid = ((cm_test_chaid[1,1]+cm_test_chaid[2,2])/1500)
Error_Test_chaid = 1-accuracty_test_chaid


## deciling
probs = seq(0,1,length = 11)
qs_test_chaid = quantile(tbank_test_chaid$score_chaid_test,probs)
print(qs_test_chaid)
tbank_test_chaid$deciles= cut(tbank_test_chaid$score_chaid_test,unique(qs_test_chaid),include.lowest = TRUE)
print(tbank_test_chaid$deciles)
head(tbank_test_chaid)

library(data.table)
testchaid = data.table(tbank_test_chaid)
ranktbl_test_chaid = testchaid[,list(cnt = length(Personal.Loan),cnt_Loan_1 = sum(Personal.Loan == 1),cnt_Loan_0 = sum(Personal.Loan == 0)), by= deciles][order(-deciles)]
print(ranktbl_test_chaid)
ranktbl_test_chaid$rrate_test = round(ranktbl_test_chaid$cnt_Loan_1/ranktbl_test_chaid$cnt,4)*100
ranktbl_test_chaid$cim_resp_test = cumsum(ranktbl_test_chaid$cnt_Loan_1)
ranktbl_test_chaid$cim_non_resp_test = cumsum(ranktbl_test_chaid$cnt_Loan_0)
ranktbl_test_chaid$cim_rel_resp_test = round(ranktbl_test_chaid$cim_resp_test/sum(ranktbl_test_chaid$cnt_Loan_1),4)*100
ranktbl_test_chaid$cim_rel_non_resp_test = round(ranktbl_test_chaid$cim_non_resp_test/sum(ranktbl_test_chaid$cnt_Loan_0),4)*100
ranktbl_test_chaid$KS_test = abs(ranktbl_test_chaid$cim_rel_resp_test - ranktbl_test_chaid$cim_rel_non_resp_test)
predobj_test_chaid = prediction(tbank_test_chaid$score_chaid_test,tbank_test_chaid$Personal.Loan)  
perf_test_chaid = performance(predobj_test_chaid,"tpr","fpr")
plot(perf_test_chaid)
ks_test_chaid = max(perf_test_chaid@y.values[[1]] - perf_test_chaid@x.values[[1]])  
print(ks_test_chaid)  

auc_test_chaid = performance(predobj_test_chaid,"auc")
auc_test_chaid = as.numeric(auc_test_chaid@y.values)

print(auc_test_chaid)  

gini_test_chaid = ineq(tbank_test_chaid$score_chaid_test,"gini")
print(gini_test_chaid)  

library("InformationValue")  
Concordance(actuals = tbank_test_chaid$Personal.Loan,predictedScores = tbank_test_chaid$score_chaid_test)  


Concordance_test_chaid =Concordance(actuals = tbank_test_chaid$Personal.Loan,predictedScores = tbank_test_chaid$score_chaid_test)  

test_chaid_key = c(auc_test_chaid,ks_test_chaid,sensitivity_test_chaid,specificity_test_chaid,accuracty_test_chaid, Error_Test_chaid, gini_test_chaid, Concordance_test_chaid) 




### RANDOM FOREST ###
##Data preperation
str(tbankfinal_train)
str(tbankfinal_test)
tbank_train_rforest = tbankfinal_train
tbank_test_rforest = tbankfinal_test
dim(tbank_train_rforest)
dim(tbank_test_rforest)
tbank_train_rforest$prediction = NULL
tbank_train_rforest$score = NULL
tbank_train_rforest$deciles = NULL
tbank_test_rforest$prediction = NULL
tbank_test_rforest$score = NULL
tbank_test_rforest$deciles = NULL
str(tbank_train_rforest)
library(randomForest)  

tbank_train_rforest$ZIP.Code = as.numeric(tbank_train_rforest$ZIP.Code)

set.seed(42)
RFmodel <- randomForest(Personal.Loan ~., data = tbank_train_rforest[,-3], type = "class", mtry = 5, 
                        nodesize = 10, ntree = 501, importance = TRUE)

RFmodel_2 <- randomForest(Personal.Loan ~., data = tbank_train_rforest, type = "class", mtry = 5, 
                        nodesize = 10, ntree = 501, importance = TRUE)

RFmodel
RFmodel_2
plot(RFmodel,main = "")  
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates")
RFmodel$err.rate
str(tbank_train_rforest)
  
tRF <- tuneRF(x = tbank_train_rforest[,-c(3,8)],
              y=tbank_train_rforest$Personal.Loan,
              mtryStart = 5,
              ntreeTry=201,
              stepFactor = 1.5,
              improve = 0.0001,
              trace=TRUE,
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10,
              importance=TRUE
)

tRF

print(tRF)
plot(tRF)
tRF$importance

##Scoring
tbank_train_rforest$prediction_rf_train<- predict(tRF, newdata = tbank_train_rforest, type="class")
tbank_train_rforest$score_rf_train<- predict(tRF, newdata = tbank_train_rforest, type="prob")[,"1"]
head(tbank_train_rforest)

##Model performance on train dataset

nrow(tbank_train_rforest)

cm_train_rf = table( tbank_train_rforest$prediction_rf_train,tbank_train_rforest$Personal.Loan)
print(cm_train_rf)

sensitivity_train_rf = cm_train_rf[2,2]/(cm_train_rf[2,2] + cm_train_rf[1,2])
specificity_train_rf = cm_train_rf[1,1]/(cm_train_rf[1,1] + cm_train_rf[2,1])
accuracty_train_rf = ((cm_train_rf[1,1]+cm_train_rf[2,2])/3500)
Error_Train_rf= 1-accuracty_train_rf

## deciling
probs = seq(0,1,length = 11)
qs_rf_train = quantile(tbank_train_rforest$score_rf_train,probs)
print(qs_rf_train)
tbank_train_rforest$deciles= cut(tbank_train_rforest$score_rf_train,unique(qs_rf_train),include.lowest = TRUE)
print(tbank_train_rforest$deciles)
head(tbank_train_rforest)

library(data.table)
trainrf = data.table(tbank_train_rforest)
ranktbl_train_rf = trainrf[,list(cnt = length(Personal.Loan),cnt_Loan_1 = sum(Personal.Loan == 1),cnt_Loan_0 = sum(Personal.Loan == 0)), by= deciles][order(-deciles)]
print(ranktbl_train_rf )
ranktbl_train_rf $rrate = round(ranktbl_train_rf$cnt_Loan_1/ranktbl_train_rf$cnt,4)*100
ranktbl_train_rf$cim_resp = cumsum(ranktbl_train_rf$cnt_Loan_1)
ranktbl_train_rf$cim_non_resp = cumsum(ranktbl_train_rf$cnt_Loan_0)
ranktbl_train_rf$cim_rel_resp = round(ranktbl_train_rf$cim_resp/sum(ranktbl_train_rf$cnt_Loan_1),4)*100
ranktbl_train_rf$cim_rel_non_resp = round(ranktbl_train_rf$cim_non_resp/sum(ranktbl_train_rf$cnt_Loan_0),4)*100
ranktbl_train_rf$KS = abs(ranktbl_train_rf$cim_rel_resp - ranktbl_train_rf$cim_rel_non_resp)
predobj_train_rf = prediction(tbank_train_rforest$score_rf_train,tbank_train_rforest$Personal.Loan)  
perf_train_rf= performance(predobj_train_rf,"tpr","fpr")
plot(perf_train_rf)
ks_train_rf = max(perf@y.values[[1]] - perf@x.values[[1]])  
print(ks_train_rf)  

auc_train_rf = performance(predobj_train_rf,"auc")
auc_train_rf = as.numeric(auc_train_rf@y.values)

print(auc_train_rf)  

gini_train_rf = ineq(tbank_train_rforest$score_rf_train,"gini")
print(gini)  

library("InformationValue")  
Concordance(actuals = tbank_train_rforest$Personal.Loan,predictedScores = tbank_train_rforest$score_rf_train)  

Concordance_train_rf = Concordance(actuals = tbank_train_rforest$Personal.Loan,predictedScores = tbank_train_rforest$score_rf_train) 

train_rf_key = c(auc_train_rf,ks_test_rf,sensitivity_train_rf,
                specificity_train_rf,accuracty_train_rf ,Error_Train_rf, gini_train_rf, Concordance_train_rf)

##Scoring on Test Dataset

tbank_test_rforest$prediction_test_rf<- predict(tRF, newdata = tbank_test_rforest, type="class")
tbank_test_rforest$score_test_rf <- predict(tRF, newdata = tbank_test_rforest,type = "prob")[,"1"]
head(tbank_test_rforest)

##Model performance on test dataset

nrow(tbank_test_rforest)

cm_test_rf = table( tbank_test_rforest$prediction_test_rf,tbank_test_rforest$Personal.Loan)
print(cm_test_rf)

sensitivity_test_rf = cm_test_rf[2,2]/(cm_test_rf[2,2] + cm_test_rf[1,2])
specificity_test_rf = cm_test[1,1]/(cm_test_rf[1,1] + cm_test_rf[2,1])
accuracty_test_rf = ((cm_test_rf[1,1]+cm_test_rf[2,2])/1500)
Error_Test_rf = 1-accuracty_test_rf 


## deciling
probs = seq(0,1,length = 11)
qs_test_rf = quantile(tbank_test_rforest$score_test_rf,probs)
print(qs_test_rf)
tbank_test_rforest$deciles= cut(tbank_test_rforest$score_test_rf,unique(qs_test_rf),include.lowest = TRUE)
print(tbank_test_rforest$deciles)
head(tbank_test_rforest)

library(data.table)
testrf = data.table(tbank_test_rforest)
ranktbl_test_rf = testrf[,list(cnt = length(Personal.Loan),cnt_Loan_1 = sum(Personal.Loan == 1),cnt_Loan_0 = sum(Personal.Loan == 0)), by= deciles][order(-deciles)]
print(ranktbl_test_rf)
ranktbl_test_rf$rrate_test = round(ranktbl_test_rf$cnt_Loan_1/ranktbl_test_rf$cnt,4)*100
ranktbl_test_rf$cim_resp_test = cumsum(ranktbl_test_rf$cnt_Loan_1)
ranktbl_test_rf$cim_non_resp_test = cumsum(ranktbl_test_rf$cnt_Loan_0)
ranktbl_test_rf$cim_rel_resp_test = round(ranktbl_test_rf$cim_resp_test/sum(ranktbl_test_rf$cnt_Loan_1),4)*100
ranktbl_test_rf$cim_rel_non_resp_test = round(ranktbl_test_rf$cim_non_resp_test/sum(ranktbl_test_rf$cnt_Loan_0),4)*100
ranktbl_test_rf$KS_test = abs(ranktbl_test_rf$cim_rel_resp_test - ranktbl_test_rf$cim_rel_non_resp_test)
predobj_test_rf = prediction(tbank_test_rforest$score_test_rf,tbank_test_rforest$Personal.Loan)  
perf_test_rf = performance(predobj_test_rf,"tpr","fpr")
plot(perf_test_rf, main = "roc curve")
ks_test_rf = max(perf@y.values[[1]] - perf@x.values[[1]])  
print(ks_test_rf)  

auc_test_rf = performance(predobj_test_rf,"auc")
auc_test_rf = as.numeric(auc_test_rf@y.values)

print(auc_test_rf)  

gini_test_rf = ineq(tbank_test_rforest$score_test_rf,"gini")
print(gini_test_rf)  

library("InformationValue")  
Concordance(actuals = tbank_test_rforest$Personal.Loan,predictedScores = tbank_test_rforest$score_test_rf)  

Concordance_test_rf = Concordance(actuals = tbank_test_rforest$Personal.Loan,predictedScores = tbank_test_rforest$score_test_rf)  
test_rf_key = c(auc_test_rf,ks_test_rf,sensitivity_test_rf,
                specificity_test_rf,accuracty_test_rf ,Error_Test_rf, gini_test_rf, Concordance_test_rf)
test_rf_key

rows = c("auc","ks","sensitivity","specificity","accuracy", "error","gini","concordance","discordance","tied","pairs")


model_performance = data.table(rows,test_rf_key,train_rf_key,test_chaid_key,train_chaid_key,test_cart_key,train_cart_key)
str(model_performance)
model_performance 
view(model_performance)








