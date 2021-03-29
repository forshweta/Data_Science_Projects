#load the libraries
library(caTools)  
library(caret)
library(ROCR)
library(corrplot)
library(car)
library(dplyr)
library(tidyverse)
library(outliers)
library(reshape2)
library(purrr)

#set working directory

setwd("/Users/shweta/Desktop/PGPBABI/finance risk analytics/project")

#read the data

comps = read.csv("/Users/shweta/Desktop/PGPBABI/finance risk analytics/project/raw-data.csv",header = T,na.strings = c("","NA"))
names(comps)
View(comps)
sum(is.na(comps))

#checking the dimensions 
dim(comps)

#check summary of data
summary(comps)

# check structure of data
str(comps)
head(comps,10)


#remove Deposits variable since all values are blank
mydata = comps[-22]
names(mydata)

#check missing values
colSums(is.na(mydata))
sum(is.na(mydata))

#remove variables with more than 30% missing values

##PE.on.bse has 62% missing values
2194/3541
##Investments has 41% missing values
1435/3541
##contingent liabilities has 34% missing values
1188/3541
##Deferred tax liabilities has 32% missing values
1140/3541
##other income has 32% missing values
1295/3541

#remove variables with 30% or more missing values

mydata = mydata[-c(19,51,24,31,33)]
names(mydata)
sum(is.na(mydata))

#check outliers

ggplot(melt(mydata[1:10]), aes(variable, value)) + geom_boxplot() +  coord_flip()
ggplot(melt(mydata[11:20]), aes(variable, value)) + geom_boxplot() +  coord_flip()
ggplot(melt(mydata[21:30]), aes(variable, value)) + geom_boxplot() +  coord_flip()
ggplot(melt(mydata[31:40]), aes(variable, value)) + geom_boxplot() +  coord_flip()
ggplot(melt(mydata[41:46]), aes(variable, value)) + geom_boxplot() +  coord_flip()

# outliers replacement with 5 percentile and 95 percentlie values

capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}


#apply the outlier function on dataset to treat outliers in all variables
new = apply(mydata,2,capOutlier)
datanew = as.data.frame(new, row.names = TRUE,col.names = TRUE)

#impute missing values using KNN imputation
library(DMwR)
sum(is.na(datanew))
data.wo.nas = knnImputation(datanew, k = 5)
sum(is.na(data.wo.nas))


#check multicollinearity
#names(data.wo.nas)
#library(corrplot)
#cormat = cor(data.wo.nas[-c(1,2)])
#write.csv(cormat, "correlation.csv")
#view(cormat)
#par(pin=c(7,7))              ##  (width, height) in inches    
#par(omi=c(0,1,1,0.5))        ## (bottom, left, top, right)  in inches  
#corrplot(cormat, method = "circle",type="upper", order = "FPC",diag = F, tl.cex=0.4)

# Feature Engineering #

# create new variables - rations as per Altmans model
##liquidity
data.wo.nas$Working.capital.to.Total.assets = (data.wo.nas$Net.working.capital)/(data.wo.nas$Total.assets)
##leverage
data.wo.nas$Retained.earnings.to.Total.assets= (data.wo.nas$Cumulative.retained.profits)/(data.wo.nas$Total.assets)
##Profitabiltiy
data.wo.nas$PBDITA.to.Total.assets= (data.wo.nas$PBDITA)/(data.wo.nas$Total.assets)
##Profitabiltiy
data.wo.nas$Net.sales.to.Total.assets= (data.wo.nas$Sales)/(data.wo.nas$Total.assets)                                     
##Size                                    
data.wo.nas$Equity.to.Total.assets= (data.wo.nas$Equity.face.value)/(data.wo.nas$Total.assets)
summary(data.wo.nas)
str(data.wo.nas)

##check multicollinearity
names(data.wo.nas)
library(corrplot)
cormat = cor(data.wo.nas[,-c(1,2)])
#par(pin=c(7,7))              ##  (width, height) in inches    
#par(omi=c(0,1,1,0.5))        ## (bottom, left, top, right)  in inches  
corrplot(cormat, method = "circle",type="upper", order = "FPC",diag = F, tl.cex=0.4)

# create a dependent variable Default using net worth next year and check proportion of dependent variable

data.wo.nas$Default = ifelse(data.wo.nas$Networth.Next.Year>0,0,1)
summary(as.factor(data.wo.nas$Default))
prop.table(table(data.wo.nas$Default))
data.wo.nas$Default = as.factor(data.wo.nas$Default)
names(data.wo.nas)

# EDA univariate and bivaraite analysis

library(ggplot2)
library(RColorBrewer)
library(GGally)
names(data.wo.nas)
pairs.panels(data.wo.nas[,c(16,51,33,35,34,41,15,24,26)], gap=0, bg=c("red","green")[data.wo.nas$Default], pch=21)

boxplot(data.wo.nas[, c(16,51,33,35,34,41,15,24,26)],las = 1,horizontal = TRUE,
        cex = 0.3,
        par(cex.axis = 0.4),
        col=brewer.pal(8,"Set1"), main = "Boxplots of significant variables")

ggpairs(data.wo.nas[,  c(16,51,33,35,34,41,15,24,26)],ggplot2::aes(colour = data.wo.nas$Default))

library(reshape2)
newdata = melt(data.wo.nas[,c(16,51,33,35,34,41,15,24,26)], id = c("Default"))
View(newdata)

ggplot(data = data.wo.nas[,c(16,51,33,35,34,41,15,24,26)], aes(x= Default, y=value)) + geom_boxplot(aes(color = Default),alpha =0.7) +facet_wrap(~variable,scales = "free_x", nrow = 5) +  coord_flip()

ggplot(data = newdata, aes(x= Default, y=value)) + geom_boxplot(aes(color = Default),alpha =0.7) +facet_wrap(~variable,scales = "free_x", nrow = 5) + coord_flip()


# WE WILL WORK WITH THREE TYPES OF DATA SETS 1. FULL DATASET 2.FACTORS ON FULL DATASET 3. DATASET WITH ONLY RATIO VARIABLES 
# RATIO DATASET
# create a new dataset with only ratios variables
names(data.wo.nas)
#ratio.dat = data.wo.nas[-c(1:5,7:11,17:25,29:31,42:43,46)]
ratio.dat2 = data.wo.nas[,c(6,12:16,26:28,32:41,44,45,47:52)]
names(ratio.dat2)
#summary of ratio data by Default
by(ratio.dat2,INDICES = ratio.dat$Default, FUN = summary)

#check multicollinearity in ratio dataset
cormat2 = cor(ratio.dat2[,- 27])
view(cormat)
#par(pin=c(7,7))              ##  (width, height) in inches    
#par(omi=c(0,1,1,0.5))        ## (bottom, left, top, right)  in inches  
corrplot(cormat2, method = "circle",type="upper", order = "FPC",diag = F, tl.cex=0.4)

#FULL DATASET FACTORS
#treat multicollinearity in full datset(all variables) using PCA

library(psych)

cortest.bartlett(cormat) # check whether the corrlatation matrix is identity matrix( diag 1 and other values o), if it is not then their is scope for dimensionality reduction

KMO(cormat) # check for sampling adequacy for doing PCA

# Treating multicollinearity in Data using PCA

A = eigen(cormat)
ev = A$values
ev
names(data.wo.nas)
library(FactoMineR)
plot(ev, xlab = "Factors", ylab="Eigen Value", pch=20, col="blue")
lines(ev, col="red")
# we go ahead with 4 factor solution
eFactors = fa(data.wo.nas[,-c(1,2,52)], nfactors=4, rotate="varimax")
eFactors
fa.diagram(eFactors, cex = 0.4)
print(eFactors)
fa.plot(eFactors)
eFactors$scores
loadings = eFactors$loadings
fa.sort(eFactors)
RotatedProfile=plot(eFactors,row.names(eFactors$loadings),cex=1.0)
attributes(eFactors)
result <- PCA(data.wo.nas[,-1])

# FULL DATASET WITH FACTORS

#Create a final dataset using PCA factors and other relevant variables for full data
final_data = as.data.frame(cbind(eFactors$scores, data.wo.nas$Default))
names(final_data) = c("Company.size","Profitability","Leverage","Liquidity","Default")
final_data$Default = as.factor(final_data$Default)
summary(final_data)

#SMOTE for data balancing on factor data
#smoted_data <- SMOTE(Default ~., final_data, perc.over=400,perc.under = 250)
#summary(smoted_data$Default)
#summary(final_data$Default)

# TEST DATASET
# Read the test data and perform similar treatment as train dataset

test = read.csv("/Users/shweta/Desktop/PGPBABI/finance risk analytics/project/validation_data.csv",header = T,na.strings = c("","NA"))
sum(is.na(test))

colSums(is.na(test))
names(test)
names(test2)
names(comps)
summary(test)
#remove deposits variable in test data
test2 = test[-22]
#remove other variables with high NAs in test data
test2 = test2[,-c(19,51,24,31,33)]
#outliers replacement with 5 percentile and 95 percentlie values in test data
test2 = apply(test2,2,capOutlier)
test2 = as.data.frame(test2, row.names = TRUE,col.names = TRUE)

#impute missing values using KNN imputation in test data
sum(is.na(test2))
dim(test2)
test.wo.na = knnImputation(test2, k = 5)
sum(is.na(test.wo.na))

# create new variables - rations as per Altmans model in Test dataset
#liquidity
test.wo.na$Working.capital.to.Total.assets = (test.wo.na$Net.working.capital)/(test.wo.na$Total.assets)
#leverage
test.wo.na$Retained.earnings.to.Total.assets= (test.wo.na$Cumulative.retained.profits)/(test.wo.na$Total.assets)
#Profitabiltiy
test.wo.na$PBDITA.to.Total.assets= (test.wo.na$PBDITA)/(test.wo.na$Total.assets)
#Profitabiltiy
test.wo.na$Net.sales.to.Total.assets= (test.wo.na$Sales)/(test.wo.na$Total.assets)                                     
#Size                                    
test.wo.na$Equity.to.Total.assets= (test.wo.na$Equity.face.value)/(test.wo.na$Total.assets)
summary(test.wo.na)
str(data.wo.nas)
str(test.wo.na)
names(test.wo.na)
names(data.wo.nas)

# WE WILL WORK WITH THREE TYPES OF TEST DATA SETS 1. FULL DATASET 2. FACTORS ON FULL DATASET 3. DATASET WITH ONLY RATIO VARIABLES 

# TEST DATA WITH ONLY RATIO VARIABLES
# create a test dataset with only ratios

ratio.dat.test = test.wo.na[-c(1:5,7:11,17:25,29:31,42:43,46)]
names(ratio.dat.test)
ratio.test.final = as.data.frame(cbind(ratio.dat.test, test.wo.na$Default...1))
names(ratio.test.final$`test.wo.na$Default...1` ) = "Default"
names(ratio.test.final)[names(ratio.test.final) == "test.wo.na$Default...1"] <- "Default"
names(ratio.test.final)


# TEST DATA WITH FACTORS 

names(test.wo.na)
# create factors for test data
eFactorstest = fa(test.wo.na[,-c(1,2)], nfactors=4, rotate="varimax")

#Create a final test dataset using PCA factor and other relevant variables
final_test = as.data.frame(cbind(eFactorstest$scores, test.wo.na$Default...1))
names(final_test) = c("Company.size","Profitability","Leverage","Liquidity","Default")
summary(final_test)
str(final_test)
str(final_data)
final_test$Default = as.factor(final_test$Default)

names(data.wo.nas)

# MODEL BUILDING

#logistic model building

##logistic model on full data with factors - model 1 is final model on full data with factors

default.model1 = glm(Default~.,data = final_data,family = binomial)

#default.model2 = glm(Default~.,data = smoted_data,family = binomial)

##logistic model on full data without factors - model 4 is final model on full data without factors

default.model3 =glm(Default~.,data = data.wo.nas[,-c(1,2)],family = binomial)
default.model4 =glm(Default ~ PAT.as...of.net.worth +Cumulative.retained.profits+TOL.TNW+Debt.to.equity.ratio..times.+Cash.to.current.liabilities..times. +Equity.to.Total.assets,data = data.wo.nas[,-c(1,2)],family = binomial)


##logistic model on ratio data

default.model6 = glm(Default~.,data = ratio.dat2,family = binomial)
default.model7 = glm(Default~ PAT.as...of.net.worth + TOL.TNW + Equity.to.Total.assets+ PBT.as...of.total.income +  EPS + Current.ratio..times. +Cash.to.current.liabilities..times.+Debt.to.equity.ratio..times. + Raw.material.turnover+Cash.profit.as...of.total.income,data = ratio.dat2,family = binomial)
default.model8 = glm(Default~ PAT.as...of.net.worth + TOL.TNW + Equity.to.Total.assets+EPS + Current.ratio..times. +Cash.to.current.liabilities..times.+Debt.to.equity.ratio..times. + Raw.material.turnover+Cash.profit.as...of.total.income,data = ratio.dat2,family = binomial)

##Model 9 is our final model in ratio data

default.model9 = glm(Default~ PAT.as...of.net.worth + TOL.TNW + Equity.to.Total.assets+ Current.ratio..times. +Cash.to.current.liabilities..times.+Debt.to.equity.ratio..times. + Raw.material.turnover+Cash.profit.as...of.total.income,data = ratio.dat2,family = binomial)
default.model10 = glm(Default~ PAT.as...of.net.worth + TOL.TNW + Equity.to.Total.assets+ Current.ratio..times. +Cash.to.current.liabilities..times.+ Raw.material.turnover+Cash.profit.as...of.total.income,data = ratio.dat2,family = binomial)

#ratio data model refinement steps
library(blorr)
both <- blr_step_aic_both(default.model6, details = TRUE)

# SUMMARY AND VIF ALL MODELS

summary(default.model1)
pR2(default.model1)
summary(default.model2)
summary(default.model3)
summary(default.model4)
pR2(default.model4)
summary(default.model5)
summary(default.model6)
summary(default.model7)
summary(default.model8)
#Model 9 final model in ratio data
summary(default.model9)
summary(default.model10)
vif(default.model1)
vif(default.model3)
vif(default.model4)
vif(default.model5)
vif(default.model6)
vif(default.model7)
vif(default.model8)
#Model 9 final model in ratio data
vif(default.model9)
#coefficients
exp(coef(default.model9))
# Probability
exp(coef(default.model9))/(1+exp(coef(default.model9)))
library(pscl)
pR2(default.model9)
vif(default.model10)

# TEST ACCURACY AND PREDICT ON TEST DATA

# predict default.model1
predTest=predict(default.model1, newdata = final_test[,-5],type = "response")

y_pred_num = ifelse(predTest>0.10,1,0)
y_pred = factor(y_pred_num, levels=c(0,1))
y_actual = final_test$Default
confusionMatrix(y_pred,y_actual,positive="1")

library(ROCR)
ROCRpred = prediction(predTest, y_actual)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)

plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7))


# predict default.model3
predTest3=predict(default.model3, newdata = test2[,-c(1,2)],type = "response")
y_pred_num3 = ifelse(predTest3>0.5,1,0)
y_pred3 = factor(y_pred_num3, levels=c(0,1))
y_actual = test2$Default...1
confusionMatrix(y_pred3,y_actual,positive="1")
names(test.wo.na)
# predict default.model4
predTest4=predict(default.model4, newdata = test.wo.na[,-c(1,2)],type = "response")
y_pred_num4 = ifelse(predTest4>0.10,1,0)
y_pred4 = factor(y_pred_num4, levels=c(0,1))
y_actual = as.factor(test2$Default...1)
confusionMatrix(y_pred4,y_actual,positive="1")
library(ROCR)
ROCRpred = prediction(predTest4, y_actual)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)

plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7))

# predict default.model10
names(ratio.test.final)
predTest10 =predict(default.model10, newdata = ratio.test.final[,-27],type = "response")
y_pred_num10 = ifelse(predTest10>0.1,1,0)
y_pred10 = factor(y_pred_num10, levels=c(0,1))
y_actual = as.factor(ratio.test.final$Default)
confusionMatrix(y_pred10,y_actual,positive="1")

library(ROCR)
ROCRpred = prediction(predTest10, y_actual)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)

plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7))

# predict default.model8
names(ratio.test.final)
predTest8 =predict(default.model8, newdata = ratio.test.final[,-27],type = "response")
y_pred_num8 = ifelse(predTest8>0.5,1,0)
y_pred8 = factor(y_pred_num8, levels=c(0,1))
y_actual = as.factor(ratio.test.final$Default)
confusionMatrix(y_pred8,y_actual,positive="1")

library(ROCR)
ROCRpred = prediction(predTest8, y_actual)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)

plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7))

#logistic model on ratio data - final model - TEST ACCURACY AND PREDICTIONS
## predict default.model9 on Development data
names(ratio.dat2)
predTest9 =predict(default.model9, newdata = ratio.dat2[,-27],type = "response")
y_pred_num9 = ifelse(predTest9>0.10,1,0)
y_pred9 = factor(y_pred_num9, levels=c(0,1))
y_actual = as.factor(ratio.dat2$Default)
confusionMatrix(y_pred9,y_actual,positive="1")

## predict default.model9 on Test data
names(ratio.test.final)
predTest9 =predict(default.model9, newdata = ratio.test.final[,-27],type = "response")
y_pred_num9 = ifelse(predTest9>0.10,1,0)
y_pred9 = factor(y_pred_num9, levels=c(0,1))
y_actual = as.factor(ratio.test.final$Default)
confusionMatrix(y_pred9,y_actual,positive="1")

library(ROCR)
ROCRpred = prediction(predTest9, y_actual)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)

plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7))

## DECILING ON MODEL 9 FINAL MODEL RATIO DATA
probs = seq(0,1,length = 11)
qs.test.ratio.log.9 = quantile(predTest9,probs)
print(qs.test.ratio.log.9)
ratio.test.final$deciles= cut(predTest9,unique(qs.test.ratio.log.9),include.lowest = TRUE)
print(ratio.test.final$deciles)
head(ratio.test.final$deciles)
library(data.table)
names(ratio.test.final)
testratio = data.table(ratio.test.final)
ranktbl = testratio[,list(cnt = length(Default),cnt_1 = sum(Default == 1),cnt_0 = sum(Default == 0)), by= deciles][order(-deciles)]
print(ranktbl)

ranktbl$defaultrate = round(ranktbl$cnt_1/ranktbl$cnt,4)*100
ranktbl$cum_default = cumsum(ranktbl$cnt_1)
ranktbl$cum_non_def = cumsum(ranktbl$cnt_0)
ranktbl$cum_rel_def = round(ranktbl$cum_default/sum(ranktbl$cnt_1),4)*100
ranktbl$cum_rel_non_def = round(ranktbl$cum_non_def/sum(ranktbl$cnt_0),4)*100
ranktbl$KS = abs(ranktbl$cum_rel_def - ranktbl$cum_rel_non_def)


print(ranktbl)
