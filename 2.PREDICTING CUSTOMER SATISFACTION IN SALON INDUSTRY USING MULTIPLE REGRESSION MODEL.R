

#Load all required libraries
library(MASS)
library(car)
library(ggplot2)
install.packages("Hmisc")
library(Hmisc)
library(corrplot)
library(dplyr)
library(rpivotTable)
library(psych)
library(nfactors)
library(xlsx)
rpivotTable(mydata)
#Load the dataset
setwd("/Users/shweta/Desktop/PGPBABI")
mydata = read.csv("/Users/shweta/Desktop/PGPBABI/ADVANCED STATISTICS/Project/Factor-Hair-Revised.csv")
# EDA
dim(mydata)
names(mydata)
head(mydata[,2:13],5)
tail(mydata,5)
str(mydata)
summary(mydata[,2:13])
shapiro.test(mydata$Satisfaction)
sum(anyNA(mydata))
boxplot(mydata[,2:13], col = c("red","green","blue","yellow","grey") , main = "BoxPlot of all variables",number.cex = 0.7)
hist(mydata)
hist.data.frame(mydata[,2:13])
plot(mydata[,2:13], pch=16, col="blue", main="Scatter Plot of all variables")
mydatacor = cor(mydata[2:13]) 
# check for multicollinearity
corrplot(mydatacor, method = "number",type = "upper")
#Perform Simple Linear regression with every independent variable
model_1 = lm(Satisfaction~ProdQual,data=mydata)    
summary(model_1,digits =5) 
model_2 = lm(Satisfaction~Ecom,data=mydata) 
summary(model_2) 
model_3 = lm(Satisfaction~TechSup,data=mydata) 
summary(model_3) 
model_4 = lm(Satisfaction~CompRes,data=mydata) 
summary(model_4)                                                                 
model_5 = lm(Satisfaction~Advertising,data=mydata) 
summary(model_5)                                                                 
model_6 = lm(Satisfaction~ProdLine,data=mydata) 
summary(model_6)         
model_7 = lm(Satisfaction~SalesFImage,data=mydata) 
summary(model_7)   
model_8 = lm(Satisfaction~ComPricing,data=mydata) 
summary(model_8) 
model_9 = lm(Satisfaction~WartyClaim,data=mydata) 
summary(model_9) 
model_10 = lm(Satisfaction~OrdBilling,data=mydata) 
summary(model_10) 
model_11 = lm(Satisfaction~DelSpeed,data=mydata) 
summary(model_11) 
#Visulaise model 4 results which is the best SLR model
qplot(mydata$CompRes, mydata$Satisfaction, data = mydata, main = "Relationship between Complaint_resolution and Satisfaction") +
  stat_smooth(method="lm", col="red") + xlab("Complaint resolution") +ylab("Satsifaction")
par(mfrow~c(2,2))
plot(model_4)

#perform mulitple linear regression with all variables
model_12_multi_linear_regression = lm(Satisfaction~., data=mydata)
summary(model_12_multi_linear_regression)
vif(model_12_multi_linear_regression)
# Barlett Sphericity Test for checking the possibility of data dimension reduction 

print(cortest.bartlett(mydatacor,nrow(mydata)))
#KMO test to check for sample adequacy for factor analysis
KMO(mydata)
# Finding out the Eigen Values and Eigen Vectors. 
factors<-eigen(mydatacor)
eigenvalues<-factors$values
eigenvectors<-factors$vectors
eigenvalues
#Plotting SCREE Graphs
plot(eigenvalues,type="lines",
     xlab="Pincipal Components",ylab="Eigen Values",col = "blue")

# Conduct PCA - rotated and unrotated
pc_unrotate<-principal(mydata[,2:12],nfactors = 4,rotate="none")
pc_rotate<-principal(mydata[,2:12],nfactors = 4,rotate="varimax")
pc_unrotate
pc_rotate
RotatedProfile=plot(pc_rotate,row.names(pc_rotate$loadings),cex=1.0)
fa.diagram(pc_rotate)

# Conduct FA - rotated and unrotated
mynewdata = mydata[,2:12]
mynewdata
summary(mynewdata)
newcor = cor(mynewdata)
factor_analysis_unrotate = fa(r=newcor,nfactors=4,rotate ="none",fm = "pa")
factor_analysis_rotate = fa(r=newcor,nfactors=4,rotate ="varimax",fm = "pa")
factor_analysis_unrotate
factor_analysis_rotate
fa.diagram(factor_analysis)
fa.diagram(pc_rotate)
pc_rotate$scores

#create a new dataframe using new factors and satisfaction variable
new_data = as.data.frame(cbind(pc_rotate$scores, mydata$Satisfaction))
summary(new_data)

# multiple linear regression analysis using factor scores
install.packages("caTools")
library(caTools)

# Split data in test and train
set.seed(2)
index = sample.split(new_data$V5, SplitRatio = 0.7)
train = subset(new_data,index== TRUE)
test = subset(new_data,index== FALSE)
dim(train)
dim(test)

MLR_FACTORS = lm(V5~.,data = train)
summary(MLR_FACTORS)
pred = predict(MLR_FACTORS,newdata = test)
pred
summary(pred)


mean(pred)
mean(new_data$V5)

# mean absolute percentage error between train and test data

mape = ((mean(pred) - mean(new_data$V5)) /mean(new_data$V5))
mape

# r square for test data
SSE = sum(pred - test$V5)^2
SST = sum((mean(test$V5)- test$V5)^2)

rsqre = 1- SSE/SST
rsqre

# plot our regression model

par(mfrow = c(2,2))
plot(MLR_FACTORS)

# save the file as rdata
save.image()
save.image(file = "regression and factor analysis project.RData")
  
  
  

