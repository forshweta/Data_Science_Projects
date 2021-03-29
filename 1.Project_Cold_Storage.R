library(readr) # for reading csv files
cold_storage_temp = read_csv("/Users/shweta/Desktop/PGPBABI/STATISTICAL METHODS AND DECISION MAKING/projects/cold storage/Cold_Storage_Temp_Data.csv")
dim(cold_storage_temp) #for identifying the number of observations and variables
View(cold_storage_temp) 
str(cold_storage_temp) # know the class of all variables
head(cold_storage_temp,10) # display first 10 rows
tail(cold_storage_temp,10) # display last 10 rows
attach(cold_storage_temp) 
names(cold_storage_temp)
summary(cold_storage_temp) # summarise all variables
cold_storage_temp$Season = as.factor(cold_storage_temp$Season) #change the class of variable to factor
cold_storage_temp$Month = as.factor(cold_storage_temp$Month)
summary(cold_storage_temp)
anyNA(cold_storage_temp) # identify missing values presence
sapply(cold_storage_temp, function(x) sum(is.na(x))) # identify total no of missing values presence by each variable
mean(Temperature) # average
sd(Temperature)  # standard deviation
var(Temperature) # variance
hist(Temperature,col = "green",main ="histogram of temperature") # histogram
table(Season,Month)
p = prop.table(table(Season,Month)) # table with proportions
library(dplyr)
temperature_by_season = cold_storage_temp %>% group_by(Season) %>% summarise(No.of.days=n(), average.temp.season=mean(Temperature,na.rm = TRUE),median.temp.season = median(Temperature,na.rm = TRUE),sd.season = sd(Temperature,na.rm = TRUE))  # create cross tables
print(temperature_by_season )
boxplot(cold_storage_temp)
boxplot(Temperature, horizontal = TRUE,col = "green")
cold_storage_temp[cold_storage_temp$Temperature>4.5,]
table(Month,Temperature)
month_by_temperatture = cold_storage_temp%>% group_by(Month) %>% summarise( days =n(), min.temp =min(Temperature),max.temp = max(Temperature),average.temp = mean(Temperature), median.temp = median(Temperature))
print(month_by_temperatture)
cold_storage_temp$Month = as.factor(cold_storage_temp$Month)
class(cold_storage_temp$Month)
str(cold_storage_temp)
boxplot(Temperature~Month,col = "blue") 

by(cold_storage_temp,INDICES = Month,FUN = summary) # get summary on all variables by month
IQR(Temperature)
outlier_range_positive_temperature = 3.3+1.5*0.8 # identify outlier

probability_under_2_degree = pnorm(2,2.963,0.508) # check probability under 2 degree
probability_over_4_degree = pnorm(4,2.963,0.508,lower.tail = FALSE) # check probability over 4 degree

 
probability_over_4_degree 


cold_storage_2 = read.csv("/Users/shweta/Desktop/PGPBABI/STATISTICAL METHODS AND DECISION MAKING/projects/cold storage/Cold_Storage_Mar2018.csv")

mean_cold_storage_2 = mean(cold_storage_2$Temperature)
median_cold_storage_2 = median(cold_storage_2$Temperature)
histogram_cold_storage_2 = hist(cold_storage_2$Temperature)
boxplot(cold_storage_2$Temperature)
mean_cold_storage_2
sd_cold_storage_2 = sd(cold_storage_2$Temperature)
sd_cold_storage_2
mu=3.9
n=35
tstat=(mean_cold_storage_2- mu)/(sd_cold_storage_2/(n^0.5)) # calculate tstat
tstat       
ttest = t.test(cold_storage_2$Temperature,mu = 3.9, conf.level = 0.90,alternative = "greater") # perform 1 tailed t test
ttest
saveRDS(cold_storage_temp, "As_RDS_cold_storage_temp_2020.RDS")
rstudioapi::documentSave()
readrds("")
