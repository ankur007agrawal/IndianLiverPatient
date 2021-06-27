
# Version: Intial draft - 1st version
# This is code will be changed further

# Note: In this code,
# 10error = False Negative
# 01error = False Positive
   

# Loading the required packages
library(dummies)
library(boot) # For K fold
library(caret)
library(ggplot2)

library(class) # For Knn

# https://www.kaggle.com/uciml/indian-liver-patient-records (Data Source)

getwd()
f<- file.choose()

data <- read.csv(f)
head(data)

# Check for any missing values in the dataset
sum(is.na(data))

# As there are four missing values lets check
# in which columns we have them
apply(data,2,function(x)sum(is.na(x)))

# We have missing values in the 'alkphos' column
str(data)
summary(data)

# No of Female & Male
table(data$Gender)
table(data$Dataset)

table(data$Gender,data$Dataset)

#Plotting the table
plot(table(data$Gender,data$Dataset), 
     ylab = "Patient & Not a Patient", 
     xlab = "Gender", 
     main = "Gender wise Analysis",
     col = c("lightblue"))

# Understanding Relations
p<- ggplot(data= data)
p+ geom_point(aes(Age, Total_Bilirubin))


#Relationship between Total Bilirubin & Direct Bilirubin


p1<- ggplot(data= data)
p1<- p1+ geom_point(aes(Total_Bilirubin, Direct_Bilirubin))
p1

#Relationship between Total Bilirubin & Direct Bilirubin


p2<- ggplot(data= data)
p2<- p2+ geom_point(aes(Total_Bilirubin, Total_Protiens))
p2

# factorizing Dataset  variable in the data
# Before that lets encode it properly
data$Dataset = ifelse(data$Dataset == 2, 0 ,1)
str(data)
data$Dataset = as.factor(data$Dataset)
contrasts(data$Dataset)
str(data)

# Getting data with out NAs
# **We can impute instead
data_clean = data[complete.cases(data),]

#Creating Training & Training set

index <-  createDataPartition(data_clean$Dataset,p= .8, times=1, list=F)

train <-  data_clean[index,]
test <- data_clean[-index,]

#Fitting a logistic regression model:

logistic<- glm(Dataset~., data=train, family='binomial')

summary(logistic)

# Predicting on the test data

pred= predict(logistic, test, type='response')
pred


pred = ifelse(pred > 0.40, 1, 0)
pred= as.factor(pred)

# Calculating the confusion matrix
matrix = confusionMatrix(pred,test$Dataset)
matrix

# K-Nearest neighbors ###########

#creating new variable for interpretability

train_knn = train

test_knn= test

# Let us create some dummy variable for Gender in both train and test
# Train data set


gender = dummy(train_knn$Gender)


train_knn$Gender = gender[,2]
str(train_knn)


#Test Data Set

gender=dummy(test_knn$Gender)
test_knn$Gender = gender[,2]
str(test_knn)


# Let us do scaling for Knn
train_scale = scale(train_knn[,-11]) # Excluding response
test_scale = scale(test_knn[,-11]) # Excluding response

# Knn Model Not sure how many neighbors to use So let us run a loop and record the metrics for each number of neighbors

Results_knn = matrix(0,nrow = 13, ncol = 4)
colnames(Results_knn) = c('Neighbors','01error','10error','Accuracy')

for(i in 1:13){
  pred_knn = knn(train = train_scale,
                 test = test_scale,
                 cl = train_knn[,11],
                 k = i + 2,
                 prob = TRUE)
  
  knn_con = confusionMatrix(data = pred_knn, reference = test_knn[,11])
  Results_knn[i,1] = i + 2
  Results_knn[i,2] = knn_con$table[2,1]
  Results_knn[i,3] = knn_con$table[1,2]
  Results_knn[i,4] = knn_con$overall[1]
}

Results_knn = as.data.frame(Results_knn)
Results_knn



