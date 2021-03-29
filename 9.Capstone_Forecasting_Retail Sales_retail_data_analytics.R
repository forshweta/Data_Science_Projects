# Set the working directory
setwd("/Users/shweta/Desktop/PGPBABI/CAPSTONE/Retail_Data_Analytics")
# Load the libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(GGally)
library(psych)

# Load the datasets and do basic checks
features = read.csv("/Users/shweta/Desktop/PGPBABI/CAPSTONE/Retail_Data_Analytics/Features data set.csv")
names(features)
summary(features)
str(features)
stores = read.csv("/Users/shweta/Desktop/PGPBABI/CAPSTONE/Retail_Data_Analytics/stores data-set.csv")
names(stores)
summary(stores)
sales = read.csv("/Users/shweta/Desktop/PGPBABI/CAPSTONE/Retail_Data_Analytics/sales data-set.csv")
names(sales)
summary(sales)

# Merge the 3 datasets as 1
sales_stores = merge(x = sales, y = stores, by = "Store", all.x = TRUE)
summary(sales_stores)
str(sales_stores)
sales_stores_features = merge(x = sales_stores, y = features, by = c("Store", "Date" ,"IsHoliday"), all.x = TRUE)
str(sales_stores_features)
summary(sales_stores_features)
sum(is.na(sales_stores_features))
attach(sales_stores_features)

# Change datatypes of variables as needed

sales_stores_features$Store = as.factor(sales_stores_features$Store)
sales_stores_features$Dept = as.factor(sales_stores_features$Dept)
sales_stores_features$Dept = as.factor(sales_stores_features$Dept)
sales_stores_features$Date <- as.Date(sales_stores_features$Date, "%d/%m/%Y")

# Create a new weight variable basis the Isholiday variable

sales_stores_features$weight = ifelse(sales_stores_features$IsHoliday == TRUE, 5,1)
str(sales_stores_features)


# Univariate Analysis

summary(sales_stores_features)

#Create a dataset with only complete cases
No.Na = sales_stores_features[complete.cases(sales_stores_features),]
sum(is.na(No.Na))
summary(No.Na)
View(No.Na)
str(No.Na)

# boxplots

boxplot_data = sales_stores_features[,c(5,7,8,9,10,11,12,13,14,15,16)]
no.na.boxplot_data = No.Na[,c(5,7,8,9,10,11,12,13,14,15,16)]

boxplot(boxplot_data[,c(5:9)], horizontal = TRUE,las = 1,col=brewer.pal(8,"Set1"),main = "boxplot of markdown variables")
names(boxplot_data)
boxplot(boxplot_data[,c(3,4,10,11)], par(cex.axis = 0.56), cex = 1,horizontal = TRUE,las = 1,col=brewer.pal(8,"Set1"),main = "boxplot of other variables
        ")
boxplot(boxplot_data[,1], cex = 1,horizontal = TRUE,las = 1,col=brewer.pal(8,"Set1"),main = "boxplot of weeklysales
        ")
boxplot(boxplot_data[,2], cex = 1,horizontal = TRUE,las = 1,col=brewer.pal(8,"Set1"),main = "boxplot of size
        ")
names(boxplot_data)

#Histograms

lapply(boxplot_data, FUN=hist)

# BIVARIATE ANALYSIS

##table_salesbystore =  sales_stores_features %>% group_by (Store) %>% summarise(sum = sum(Weekly_Sales))
##library(data.table)
##setDT(sales_stores_features)[, list(Weekly_Sales= sum(Weekly_Sales)) , by = Store]
##print(table_salesbystore)

table_store =  sales_stores_features %>% group_by (Store) %>% summarise(sum = sum(Weekly_Sales),avg_CPI = mean(CPI),avg_unemployment = mean(Unemployment), avg_fuelprice = mean(Fuel_Price),size = mean(Size),avg_temp = mean(Temperature),avg_markdown1 = mean(MarkDown1,na.rm = TRUE),avg_markdown2 = mean(MarkDown2,na.rm = TRUE),avg_markdown3 = mean(MarkDown3,na.rm = TRUE),avg_markdown4 = mean(MarkDown4,na.rm = TRUE),avg_markdown5 = mean(MarkDown5,na.rm = TRUE))
print(table_store)

table_dept =  sales_stores_features %>% group_by (Dept) %>% summarise(sum = sum(Weekly_Sales),avg_CPI = mean(CPI),avg_unemployment = mean(Unemployment), avg_fuelprice = mean(Fuel_Price),size = mean(Size),avg_temp = mean(Temperature),avg_markdown1 = mean(MarkDown1,na.rm = TRUE),avg_markdown2 = mean(MarkDown2,na.rm = TRUE),avg_markdown3 = mean(MarkDown3,na.rm = TRUE),avg_markdown4 = mean(MarkDown4,na.rm = TRUE),avg_markdown5 = mean(MarkDown5,na.rm = TRUE))
print(table_dept)

table_store_type =  sales_stores_features %>% group_by (Type) %>% summarise(sum_Weekly_Sales = sum(Weekly_Sales),avg_CPI = mean(CPI),avg_unemployment = mean(Unemployment), avg_fuelprice = mean(Fuel_Price),size = mean(Size),avg_temp = mean(Temperature),avg_markdown1 = mean(MarkDown1,na.rm = TRUE),avg_markdown2 = mean(MarkDown2,na.rm = TRUE),avg_markdown3 = mean(MarkDown3,na.rm = TRUE),avg_markdown4 = mean(MarkDown4,na.rm = TRUE),avg_markdown5 = mean(MarkDown5,na.rm = TRUE))
print(table_store_type)            

table_Is_holiday =  sales_stores_features %>% group_by (IsHoliday) %>% summarise(sum_Weekly_Sales = sum(Weekly_Sales),avg_CPI = mean(CPI),avg_unemployment = mean(Unemployment), avg_fuelprice = mean(Fuel_Price),size = mean(Size),avg_temp = mean(Temperature),avg_markdown1 = mean(MarkDown1,na.rm = TRUE),avg_markdown2 = mean(MarkDown2,na.rm = TRUE),avg_markdown3 = mean(MarkDown3,na.rm = TRUE),avg_markdown4 = mean(MarkDown4,na.rm = TRUE),avg_markdown5 = mean(MarkDown5,na.rm = TRUE))
print(table_Is_holiday) 


# treat outliers
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

#apply the outlier function on dataset to treat outliers in all variables
new = apply(boxplot_data,2,capOutlier)
datanew = as.data.frame(new, row.names = TRUE,col.names = TRUE)

#apply the outlier function on nona data
no.na.no.outlier = apply(no.na.boxplot_data,2,capOutlier)
no.na.2 = as.data.frame(no.na.no.outlier, row.names = TRUE,col.names = TRUE)


# treat missing values in original data
Datanew2 <- lapply(datanew, function(x) {
  if(is.numeric(x)) replace(x, is.na(x), mean(x, na.rm=TRUE))
  else x
})
sum(is.na(Datanew2))
str(Datanew2)
Datanew2 = data.frame(Datanew2)

##check multicollinearity
library(corrplot)
cormat = cor(Datanew2)
#par(pin=c(7,7))              ##  (width, height) in inches    
#par(omi=c(0,1,1,0.5))        ## (bottom, left, top, right)  in inches  
corrplot(cormat, method = "number",type="upper", order = "FPC",diag = F, tl.cex=0.4)

library(DataExplorer)
plot_correlation(sales_stores_features[,c(3,5,6)])

names(sales_stores_features)
write.csv(sales_stores_features,"sales_stores_features.csv")
write.csv(table_store_type,"storetype.csv")
write.csv(table_Is_holiday,"isholiday.csv")
write.csv(table_store,"store.csv")
write.csv(table_dept,"dept.csv")
names(sales_stores_features)


# full data

names(Datanew2)
names(sales_stores_features)
Datafull = cbind(Datanew2,sales_stores_features[,c(1,2,3,4,6,17)])
str(Datafull)
names(Datafull)
sum(is.na(Datafull))

#full data with no NA
Datafull.nona = cbind(no.na.2,No.Na[,c(1,2,3,4,6,17)])
str(Datafull.nona)

#clustering
library(cluster)
clust = kmeans(x=Datafull[,-c(12,13,14,15,16,1)], centers = 3, nstart = 2)
print(clust)
clusplot(Datafull[,-c(12,13,14,15,16,1)], clust$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)

library(NbClust)
set.seed(seed) 


totWss=rep(0,5)
for(k in 1:5){
  set.seed(seed)
  clust=kmeans(x=Datafull[,-c(12,13,14,15,16,1)], centers=k, nstart=3)
  totWss[k]=clust$tot.withinss
}
plot(c(1:5), totWss, type="b", xlab="Number of Clusters",
     ylab="sum of 'Within groups sum of squares'")  

#Data partitioning
library(caTools)
set.seed(100)
split = sample.split(Datafull, SplitRatio = 0.70) 
train = subset(Datafull, split==TRUE)
test = subset(Datafull, split==FALSE)
dim(train)
dim(test)

#Data partitioning NoNA
library(caTools)
set.seed(100)
split = sample.split(Datafull.nona, SplitRatio = 0.70) 
train2 = subset(Datafull.nona, split==TRUE)
test2 = subset(Datafull.nona, split==FALSE)
dim(train2)
dim(test2)


names(train)

#model buliding

##Linear regression on full data##

linearreg01 = lm(Weekly_Sales ~., data = train)
summary(linearreg01)

linearreg02 = lm(Weekly_Sales ~.- Temperature -Type -MarkDown4-weight-CPI-MarkDown1-Date, data = train)
summary(linearreg02)
linearreg02 = lm(Weekly_Sales ~.- Temperature -Type -MarkDown4-weight-CPI-MarkDown1-Date, data = test)
summary(linearreg02)

linearreg03 = lm(Weekly_Sales ~. -MarkDown1-Date, data = train)
summary(linearreg03)

#linear regression on full data based on step backward algorithm
linearreg04 = lm(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown2 + MarkDown3 + 
  MarkDown4 + MarkDown5 + CPI + Unemployment + Store + Date + 
  IsHoliday + Dept,data = train)
summary(linearreg04)
# predictions on Test and Train

pred01 <- predict(linearreg01,data=train)
pred02 <- predict(linearreg02,data=train)
pred03 <- predict(linearreg01,data=test)
pred04 <- predict(linearreg02,data=test)


# Model performance measures on test and train

library("Metrics")
mse01 <- mse(train$Weekly_Sales,pred01)
rmse01= sqrt(mse01)
mape01 <- mape(train$Weekly_Sales,pred01) # Mean % error
mape01
mse02 <- mse(train$Weekly_Sales,pred02)
rmse02= sqrt(mse02)
mape02 <- mape(train$Weekly_Sales,pred02) # Mean % error
mape02
mse03 <- mse(test$Weekly_Sales,pred03)
rmse03= sqrt(mse03)
mape03 <- mape(test$Weekly_Sales,pred03) # Mean % error
mape03
mse04 <- mse(test$Weekly_Sales,pred04)
rmse04 = sqrt(mse04)
mape04 <- mape(test$Weekly_Sales,pred04) # Mean % error
mape04


##Linear regression on NO NA data##

linearreg1 = lm(Weekly_Sales ~., data = train2)
summary(linearreg1)

linearreg2 = lm(Weekly_Sales ~. -Type -Size -MarkDown1-MarkDown4-MarkDown5-weight, data = train2)
summary(linearreg2)

linearreg2 = lm(Weekly_Sales ~. -Type -Size -MarkDown1-MarkDown4-MarkDown5-weight, data = test2)
summary(linearreg2)

# predictions on Test and Train

pred1 <- predict(linearreg1,data=train2)
pred2 <- predict(linearreg2,data=train2)
pred3 <- predict(linearreg1,data=test2)
pred4 <- predict(linearreg2,data=test2)


# Model performance measures on test and train

library("Metrics")
mse1 <- mse(train2$Weekly_Sales,pred1)
rmse1= sqrt(mse1)
mape1 <- mape(train2$Weekly_Sales,pred1) # Mean % error
mape1
mse2 <- mse(train2$Weekly_Sales,pred2)
rmse2= sqrt(mse2)
mape2 <- mape(train2$Weekly_Sales,pred2) # Mean % error
mape2
mse3 <- mse(test2$Weekly_Sales,pred3)
rmse3= sqrt(mse3)
mape3 <- mape(test2$Weekly_Sales,pred3) # Mean % error
mape3
mse4 <- mse(test2$Weekly_Sales,pred4)
rmse4 = sqrt(mse4)
mape4 <- mape(test2$Weekly_Sales,pred4) # Mean % error
mape4

##SVM regression on full data ## could not run## memory issues
library(e1071)

SVMreg1 = svm(Weekly_Sales ~., data = train)
summary(SVMreg1)


##SVM regression on No na data ##
library(e1071)

SVMreg1 = svm(Weekly_Sales ~., data = train2)
summary(SVMreg1)

SVMreg2 = svm(Weekly_Sales ~.- Temperature -Type -Date -MarkDown1-MarkDown4-MarkDown5,, data = train2)

# predictions on Test and Train

pred21 <- predict(SVMreg1,data=train2)
pred22 <- predict(SVMreg2,data=train2)
pred23 <- predict(SVMreg1,data=test2)
pred24 <- predict(SVMreg2,data=test2)
View(pred23)
View(test2$Weekly_Sales)

# Model performance measures on test and train

library("Metrics")
library(caret)
mse21 <- mse(train2$Weekly_Sales,pred21)
rmse21= sqrt(mse21)
mape21 <- mape(train2$Weekly_Sales,pred21) # Mean % error
mape21
mse22 <- mse(train2$Weekly_Sales,pred22)
rmse22= sqrt(mse22)
mape22 <- mape(train2$Weekly_Sales,pred22) # Mean % error
mape22
mse23 <- mse(test2$Weekly_Sales,pred23)
rmse23= sqrt(mse23)
mape23 <- mape(test2$Weekly_Sales,pred23) # Mean % error
mape23
mse24 <- mse(test2$Weekly_Sales,pred24)
rmse24 = sqrt(mse24)
mape24 <- mape(test2$Weekly_Sales,pred24) # Mean % error
mape24
rsquare.test.24 =cor(test2$Weekly_Sales,pred23)^2
rsquare.train.24 =cor(train2$Weekly_Sales,pred21)^2
R2 = R2(test2$Weekly_Sales, pred23)
r2 = R2(train2$Weekly_Sales, pred21)
MAE = mae(train2$Weekly_Sales, pred21)

# perform a grid search # could not run

tuneResult <- tune(svm, Weekly_Sales ~., data = train2,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

print(tuneResult)
# Draw the tuning graph
plot(tuneResult)


# Feature selection in linear regression

library(MASS)
step_fwd <- stepAIC(linearreg1,direction = "forward")
summary(step_fwd)

step_bwd <- stepAIC(linearreg1,direction = "backward")
summary(step_bwd)
# a high level flow of how it did the backward cal
step_bwd$anova

step_both <- stepAIC(linearreg1,direction = "both")
summary(step_both)


#other models - Bagged CART
library(caret)
library(ipred)
library(plyr)
data_ctrl <- trainControl(method = "cv", number = 5)
model_caret <- train(Weekly_Sales ~.,data = train2,trControl = data_ctrl,method = "treebag")

model_caret$finalModel
summary(model_caret)
model_caret$results
# predictions on Test and Train

pred31 <- predict(model_caret,newdata=train2)
pred32 <- predict(model_caret,newdata=test2)

rsquare.test.11 =cor(test2$Weekly_Sales,pred32)^2
rsquare.train.12 =cor(train2$Weekly_Sales,pred31)^2




#Performance measures on train and test

mse31 <- mse(train2$Weekly_Sales,pred31)
rmse31= sqrt(mse31)
mse32 <- mse(test2$Weekly_Sales,pred32)
rmse32= sqrt(mse32)


#decision trees - Cart
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(data.table)
library(ROCR)
#Setting the control parameters
r.ctrl = rpart.control(minsplit=20, cp = 0, xval = 5)

#Building the CART model
m2 <- rpart(formula = Weekly_Sales~., data = train2, control = r.ctrl)
predict.cart <- predict(m2, train2)
predict.cart.test <- predict(m2, test2)
mse51 <- mse(train2$Weekly_Sales,predict.cart)
rmse51 = sqrt(mse51)
mse52 <- mse(test2$Weekly_Sales,predict.cart.test)
rmse52 = sqrt(mse52)

#Building the CART model on full data - best model so far
r.ctrl = rpart.control(minsplit=20, cp = 0.98, xval = 5)
r.ctrl2 = rpart.control(minsplit=100,minbucket =30, cp = 0, xval = 10)
m1 <- rpart(formula = Weekly_Sales~., data = train, control = r.ctrl)
summary(m1)
printcp(m1)
m3 <- rpart(formula = Weekly_Sales~., data = train, control = r.ctrl)
m4 <- rpart(formula = Weekly_Sales~., data = train, control = r.ctrl2)
print(m4)
printcp(m4)
m4$cptable[,'xerror']
min(m4$cptable[,'xerror'])
m4$cptable[which.min(m4$cptable[,'xerror']),"CP"]
bestcp<-m4$cptable[which.min(m4$cptable[,'xerror']),"CP"]
pruned_tree = prune(m4,cp = bestcp,"CP")
printcp(pruned_tree)
fancyRpartPlot(pruned_tree,uniform = TRUE,main = "Pruned Tree")
predict.cart1 <- predict(m1, train)
predict.cart.test1 <- predict(m1, test)
predict.cart2 <- predict(m3, train)
predict.cart.test2 <- predict(m3, test)
predict.cart.test3 <- predict(m4, test)
predict.cart.train3 <- predict(m4, train)
mse61 <- mse(train$Weekly_Sales,predict.cart1)
rmse61 = sqrt(mse61)
mse62 <- mse(test$Weekly_Sales,predict.cart.test1)
rmse62 = sqrt(mse62)
rsquare.test.34 =cor(test$Weekly_Sales,predict.cart.test1)^2
rsquare.train.34 =cor(train$Weekly_Sales,predict.cart1)^2
rsquare.test.35 =cor(test$Weekly_Sales,predict.cart.test2)^2
rsquare.train.35 =cor(train$Weekly_Sales,predict.cart2)^2
rsquare.test.prunedtree =cor(test$Weekly_Sales,predict.cart.test3)^2
rsquare.train.prunedtree =cor(train$Weekly_Sales,predict.cart.train3)^2
names(train2)
msepruned <- mse(test$Weekly_Sales,predict.cart.test3)
rmsepruned<-sqrt(msepruned)

#Building Random Forest
set.seed(42)
library(randomForest) 

RFmodel_1 <- randomForest(Weekly_Sales~., data = train2[,-c(12,15)], mtry = 5, 
                          nodesize = 500, ntree = 1000, importance = TRUE)
 
tRF <- tuneRF(x = train2[,-c(12,15)],
              y=train2$Weekly_Sales,
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



RFmodel_2 <- randomForest(Weekly_Sales~., data = train2[,-c(12,15)], mtry = 3, 
                          nodesize = 300, ntree = 1000, importance = TRUE)
RFmodel_1$importance
     
predict.rf <- predict(RFmodel_1, train2)
predict.rf.test <- predict(RFmodel_1, test2)
mse71 <- mse(train2$Weekly_Sales,predict.rf)
rmse71 = sqrt(mse71)
mse72 <- mse(test2$Weekly_Sales,predict.rf.test)
rmse72 = sqrt(mse72)
rsquare.test.72 =cor(test2$Weekly_Sales,predict.rf.test)^2
rsquare.train.72 =cor(train2$Weekly_Sales,predict.rf )^2

predict.rf2 <- predict(RFmodel_2, train2)
predict.rf.test2 <- predict(RFmodel_2, test2)
mse81 <- mse(train2$Weekly_Sales,predict.rf2)
rmse81 = sqrt(mse81)
mse82 <- mse(test2$Weekly_Sales,predict.rf.test2)
rmse82 = sqrt(mse82)
rsquare.test.82 =cor(test2$Weekly_Sales,predict.rf.test2)^2
rsquare.train.82 =cor(train2$Weekly_Sales,predict.rf2 )^2

#Tuning Random Forest

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Weekly_Sales~., data = train2[,-c(12,15)], method="rf", tuneLength=10, trControl=control)
print(rf_random)
plot(rf_random)
RFmodel_1 <- randomForest(Weekly_Sales~., data = train2[,-c(12,15)], mtry = 5, 
                          )
#Building XGBoost
library(xgboost)
str(test2)
### Model Building - Boosting
##XGBoost works with matrices that contain all numeric variables
# we also need to split the training data and label
names(train3)
str(train3)
train.new()
train3 <- data.frame(lapply(train2[-c(13,14,16)], function(x) as.numeric(as.character(x))))
test3 <- data.frame(lapply(test2[-c(13,14,16)], function(x) as.numeric(as.character(x))))
str(train3)


gd_features_train<-as.matrix(train3[,-1])
gd_label_train<-as.matrix(train3[,1])
gd_features_test<-as.matrix(test3[,-1])

xgb.fit <- xgboost(
  data = gd_features_train,
  label = gd_label_train,
  eta = 0.9,
  max_depth = 50000,
  min_child_weight = 3,
  nrounds = 2,
  nfold = 10,
  objective = "reg:linear", 
  verbose = 0,               
  early_stopping_rounds = 10 
)

summary(xgb.fit)
xgb.pred.train <- predict(xgb.fit, gd_features_train)
xgb.pred.test <- predict(xgb.fit, gd_features_test)

mse91 <- mse(train2$Weekly_Sales,xgb.pred.train)
rmse91 = sqrt(mse91)
mse92 <- mse(test2$Weekly_Sales,xgb.pred.test)
rmse92 = sqrt(mse92)
rsquare.test.92 =cor(test2$Weekly_Sales,xgb.pred.test)^2
rsquare.train.92 =cor(train2$Weekly_Sales,xgb.pred.train)^2

#Tuning XGBoost
library(Metrics)
tp_xgb<-vector()
lr <- c(0.001, 0.01, 0.1, 0.3, 0.5, 0.7,0.9, 1)
md<-c(1000,5000,10000,30000,50000)
nr<-c(2, 50, 100, 1000, 10000)
for (i in md) {
  xgb.fit <- xgboost(
    data = gd_features_train,
    label = gd_label_train,
    eta = 0.9,
    max_depth = md,
    nrounds = 2,
    nfold = 5,
    objective = "reg:linear", 
    verbose = 0,               
    early_stopping_rounds = 10 
  )
  
  xgb.pred <- predict(xgb.fit, gd_features_test)
  rmse0<- sqrt(mse(test2$Weekly_Sales,xgb.pred))
  tp_xgb<-cbind(tp_xgb,rmse0)

print(rmse0)
  
}

tp_xgb


#XGboost on full data

train4 <- data.frame(lapply(train[-c(13,14,16)], function(x) as.numeric(as.character(x))))
test4 <- data.frame(lapply(test[-c(13,14,16)], function(x) as.numeric(as.character(x))))

gd_features_train2<-as.matrix(train4[,-1])
gd_label_train2<-as.matrix(train4[,1])
gd_features_test2<-as.matrix(test4[,-1])

xgb.fit2 <- xgboost(
  data = gd_features_train2,
  label = gd_label_train2,
  eta = 0.9,
  max_depth = 50000,
  min_child_weight = 3,
  nrounds = 2,
  nfold = 10,
  objective = "reg:linear", 
  verbose = 0,               
  early_stopping_rounds = 10 
)

summary(xgb.fit2)
xgb.pred.train2 <- predict(xgb.fit2, gd_features_train2)
xgb.pred.test2<- predict(xgb.fit2, gd_features_test2)

mse99 <- mse(train$Weekly_Sales,xgb.pred.train2)
rmse99 = sqrt(mse99)
mse55 <- mse(test$Weekly_Sales,xgb.pred.test2)
rmse55 = sqrt(mse55)
rsquare.test.99 =cor(test$Weekly_Sales,xgb.pred.test2)^2
rsquare.train.99 =cor(train$Weekly_Sales,xgb.pred.train2)^2

# caret package 


data_ctrl <- trainControl(method = "cv", number = 5)
model_caret <- train(Weekly_Sales ~.,data = train2,trControl = data_ctrl,method = "rf",mtry = 2, nodesize = 300, ntree = 1000,importance = TRUE) )
