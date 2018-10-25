
library(readr)
library(caret)
library(tidyverse)
library(rpart)
library(ISLR)
library(tree)
library(haven)
library(fastDummies)
library(corrplot)
library(Hmisc)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(imputeMissings)
data = read_sas('C:\\Users\\Debosmita\\Documents\\Data Mining HW2\\fftsales.sas7bdat')

#check value distribution
summary(data)
unique(data$default)
unique(data$loan)
unique(data$val_email)
unique(data$mortgage)
unique(data$last_campaign_outcome)

#check counts of each categorical variables
aggregate(data.frame(count = data$default), list(value = data$default), length)#1.8% yes
aggregate(data.frame(count = data$loan), list(value = data$loan), length)#1.6% yes
aggregate(data.frame(count = data$val_email), list(value = data$val_email), length)#19% yes
aggregate(data.frame(count = data$mortgage), list(value = data$mortgage), length)# good distribution
aggregate(data.frame(count = data$last_campaign_outcome), list(value = data$last_campaign_outcome), length)#(only 18% have info)
aggregate(data.frame(count = data$education), list(value = data$education), length)# good distribution
aggregate(data.frame(count = data$job), list(value = data$job), length) # good distribution
aggregate(data.frame(count = data$balance), list(value = data$balance), length)#838 have 0
aggregate(data.frame(count = data$phone_type), list(value = data$phone_type), length)#13020 unknown
aggregate(data.frame(count = data$ConsumerGroup), list(value = data$ConsumerGroup), length)#good distribution
aggregate(data.frame(count = data$total_contacts), list(value = data$total_contacts), length)#36954 has never been contacted
aggregate(data.frame(count = data$last_campaign_outcome), list(value = data$last_campaign_outcome), length)#36959 unknown value



#changing binary yes/no column values to 1/0 -total 5 categorical variables - consumer_group is not here
data$default<- ifelse(data$default=="yes", 1, 0)
data$loan<- ifelse(data$loan=="yes", 1, 0)
data$val_email<- ifelse(data$val_email=="yes", 1, 0)
data$mortgage<- ifelse(data$mortgage=="yes", 1, 0)

#additional observations
#in days_since column: 36954 records out of 45211= -1(only 18% have info)


######################################### New Analysis #########################################################
#replacing all unknown values with NA there were some values which has "unknown" values - replacing them as NA so that
# they get treated as missing values
data$education<- ifelse(data$education=="unknown", NA,data$education)
data$job<- ifelse(data$job=="unknown", NA,data$education)

#dropping unnecessary cols
drops <- c("phone_type","total_contacts","days_since","last_campaign_outcome","day","month")
mod_data = data[ , !(names(data) %in% drops)]

colnames(mod_data)

# tree can work on a data frame where each column is a factor. so converting the dataframe with all columns as a factor
mod_data <- as.data.frame(unclass(mod_data))
str(mod_data)


## 75% of the sample size
smp_size <- floor(0.75 * nrow(mod_data))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(mod_data)), size = smp_size)
#split data
train <- mod_data[train_ind, ]
test <- mod_data[-train_ind, ]

#DECISION TREE
tree.train = rpart(train$y ~., data=mod_data, method = "class")
plot(tree.train)
text(tree.train)

# Time to plot your fancy tree
fancyRpartPlot(tree.train)

# predicting data
prediction <- predict(tree.train, newdata = test, type = "class")

# confusion matrix and accuracy
confMat <- table(test$y,prediction)
accuracy <- sum(diag(confMat))/sum(confMat)



