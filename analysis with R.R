
walmart<-read.csv(file = "C:/srijon410/walmart.csv")
store5_data <- walmart[walmart$Store == 5,]
####################1##############
max_sales_date <- store5_data$Date[which.max(store5_data$Weekly_Sales)]
print(max_sales_date)


min_sales_date <- store5_data$Date[which.min(store5_data$Weekly_Sales)]
print(min_sales_date)


###########2)
mean_weekly_sales <- mean(store5_data$Weekly_Sales)
print(mean_weekly_sales)

sd_weekly_sales <- sd(store5_data$Weekly_Sales)
print(sd_weekly_sales)


###########3)
boxplot(store5_data$Weekly_Sales, main = "Weekly Sales Boxplot")


#############4)
plot(store5_data$Temperature, store5_data$Weekly_Sales, 
     main="Scatterplot: Weekly Sales vs. Temperature", 
     xlab="Temperature", ylab="Weekly Sales")



############5############
library(ggplot2)
ggplot(store5_data, aes(x = Date, y = Weekly_Sales, group = 1)) +
  geom_line() +
  labs(title = "Weekly Sales Over Time", x = "Date", y = "Weekly_Sales")
ggplot(store5_data, aes(x = Date, y = Weekly_Sales, group = 2)) +
  geom_line() +
  labs(title ="                  Weekly Sales Over Time", x = "Date", y = "Weekly_Sales")


############6############


walmart<-read.csv(file = "C:/srijon410/walmart.csv")
store5_data <- walmart[walmart$Store == 5,]


####@2####


library(dplyr)
store5_data$Fuel_Price.l1<- dplyr::lag(store5_data$Fuel_Price, n=1)
store5_data$Fuel_Price.l2<- dplyr::lag(store5_data$Fuel_Price, n=2)
store5_data$Fuel_Price.l3<- dplyr::lag(store5_data$Fuel_Price, n=3)
store5_data$Fuel_Price.l4<- dplyr::lag(store5_data$Fuel_Price, n=4)
store5_data$Fuel_Price.l5<- dplyr::lag(store5_data$Fuel_Price, n=5)
head(store5_data, 6)




store5_data$CPI.l1<- dplyr::lag(store5_data$CPI, n=1)
store5_data$CPI.l2<- dplyr::lag(store5_data$CPI, n=2)
store5_data$CPI.l3<- dplyr::lag(store5_data$CPI, n=3)
store5_data$CPI.l4<- dplyr::lag(store5_data$CPI, n=4)
store5_data$CPI.l5<- dplyr::lag(store5_data$CPI, n=5)
head(store5_data, 6)

store5_data$Unemployment.l1<- dplyr::lag(store5_data$Unemployment, n=1)
store5_data$Unemployment.l2<- dplyr::lag(store5_data$Unemployment, n=2)
store5_data$Unemployment.l3<- dplyr::lag(store5_data$Unemployment, n=3)
store5_data$Unemployment.l4<- dplyr::lag(store5_data$Unemployment, n=4)
store5_data$Unemployment.l5<- dplyr::lag(store5_data$Unemployment, n=5)
head(store5_data, 6)


store5_data$Holiday_Flag.l1<- dplyr::lag(store5_data$Holiday_Flag, n=1)
store5_data$Holiday_Flag.l2<- dplyr::lag(store5_data$Holiday_Flag, n=2)
store5_data$Holiday_Flag.l3<- dplyr::lag(store5_data$Holiday_Flag, n=3)
store5_data$Holiday_Flag.l4<- dplyr::lag(store5_data$Holiday_Flag, n=4)
store5_data$Holiday_Flag.l5<- dplyr::lag(store5_data$Holiday_Flag, n=5)
head(store5_data, 6)


clean.store5_data<- na.omit( store5_data )
head(clean.store5_data)

#########Random Forest###########

library(randomForest)
library(caret)

# Do we need all features?
rf_classifier = randomForest(Weekly_Sales~ Fuel_Price.l1 + Fuel_Price.l2 + Fuel_Price.l3 + Fuel_Price.l4 + Fuel_Price.l5 + Unemployment.l1 + Unemployment.l2 + Unemployment.l3 + Unemployment.l4 + Unemployment.l5 + CPI.l1 + CPI.l2 + CPI.l3 + CPI.l4 + CPI.l5 + Holiday_Flag.l1 + Holiday_Flag.l2 + Holiday_Flag.l3 + Holiday_Flag.l4 + Holiday_Flag.l5, data=clean.store5_data, ntree=300, importance=TRUE)
i_scores <- varImp(rf_classifier, conditional=TRUE)

fis.dframe<- data.frame(scores = i_scores, features = row.names(i_scores))
fis.dframe<- fis.dframe[ order(fis.dframe[,1], decreasing = TRUE), ] # ordered by score
fis.dframe$order<- 1:nrow(fis.dframe) # order


# plot the importance score by ggplot

library(ggplot2)
e1 <- ggplot(data=fis.dframe, aes(x = reorder(features, order), y = Overall)) + 
  geom_bar(stat="identity", width=0.5, position=position_dodge())

p1 <- e1 + theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "green"),
        axis.text.x = element_text(size=9, angle = 90, hjust = 1, colour = "red"),
        axis.text.y = element_text(size=9, colour = "black"), 
        axis.title = element_text(size=9)) + 
  xlab("Variables") + 
  ylab("Relative Importance")

p1


# plot the importance score
barplot(fis.dframe$Overall, names.arg = fis.dframe$features )
box()



#nrow(clean.store_data)
library(randomForest)
trainData = clean.store5_data[1:(nrow(clean.store5_data)-4), ] 
testData = clean.store5_data[(nrow(clean.store5_data)-4+1):nrow(clean.store5_data), ] # this can be new data

rf_classifier = randomForest(Weekly_Sales~ Fuel_Price.l1 + Fuel_Price.l2 + Fuel_Price.l3 + Fuel_Price.l4 + Fuel_Price.l5 + Unemployment.l1 + Unemployment.l2 + Unemployment.l3 + Unemployment.l4 + Unemployment.l5 + CPI.l1 + CPI.l2 + CPI.l3 + CPI.l4 + CPI.l5 + Holiday_Flag.l1 + Holiday_Flag.l2 + Holiday_Flag.l3 + Holiday_Flag.l4 + Holiday_Flag.l5, data=clean.store5_data, ntree=300, importance=TRUE)
predict.price = predict(rf_classifier, testData) # prediction

mse = mean(( testData$Weekly_Sales - predict.price )^2) # evaluation 
mse

for.out<- data.frame(original = testData$Weekly_Sales, forecast = predict.price)
for.out 


###############packages################
install.packages("caret")
install.packages("caTools")
install.packages("ggplot2")
install.packages("pROC")
install.packages("RMySQL")
install.packages("RMariaDB")
install.packages("DBI")
install.packages("dbplyr")
install.packages("tidyverse")
install.packages("shiny")
install.packages("shinydashboard")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("tseries")
install.packages("forecast")
install.packages("lubridate")
install.packages("randomForest")
install.packages("cluster")

