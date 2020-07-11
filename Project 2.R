# Group members:Vivek Shah, Shikhar Chawla, Siddhanth Vashisht

#part 1
#1:
library(ISLR)
set.seed(8)
Black_friday_data= read.csv("BlackFriday.csv")
train=sample(1:537577,430062)
training_set= Black_friday_data[train,]
attach(Black_friday_data)

#2:
Black_friday_data$Gender = as.factor(Gender)
Black_friday_data$Occupation=as.factor(Occupation)
Black_friday_data$Marital_Status=as.factor(Marital_Status)
Black_friday_data$Product_Category_1=as.factor(Product_Category_1)

lm.fit=lm(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+ Product_Category_1,data=Black_friday_data,subset=train) #fit linear reg. only using train set

summary(lm.fit)

#3:
#Gender, Product category_1, Marital_Status,City_Category and occupation are significant predictors of purchase because their p values are less than alpha=0.05.

#4:
x=mean((Purchase-predict(lm.fit,Black_friday_data))[-train]^2) # MSE for test set
y=mean(lm.fit$residuals^2) # MSE for training set
RMSE_x = sqrt(x)
RMSE_y = sqrt(y)     

#5:

# k-Fold Cross-Validation
library(boot)
set.seed(8)
glm.fit=glm(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+
              Product_Category_1,data=Black_friday_data)
cv.error.5=cv.glm(Black_friday_data,glm.fit,K=5)$delta[1]   
cv.error.5
RMSE_cv.error.5 = sqrt(cv.error.5)
RMSE_cv.error.5
detach(Black_friday_data)
# Hold out error value and cross validation RMSE are very close to each other, but cross validation RMSE is smaller than holdout error value , hence we will use 5-fold cross validation.

#Part 2
# 6:
Black_friday_data1= read.csv("BlackFriday.csv")
attach(Black_friday_data)
hist(Product_Category_1)

#7:
table(Product_Category_1)

#8:
s=sum(Product_Category_1)
probabilityArr=rep(0,18)

for(i in 1:18){
  probabilityArr[i] =  sum(Product_Category_1==toString(i))/s 
}

#Question 9:

for(i in 1:nrow(Black_friday_data)){
  if(probabilityArr[as.numeric(Product_Category_1[i])]<0.03){
    Black_friday_data$Product_Category_1[i] = "Low Probability Level"
   
  } else {
    Black_friday_data$Product_Category_1[i] = "High Probability Level"
  
  }
}
#10:
Probability_level=rep(0,nrow(Black_friday_data))
for(i in 1:nrow(Black_friday_data)){
  if(Black_friday_data$Product_Category_1[i] == "Low Probability Level"){
    Probability_level[i]=0
  } else{
    Probability_level[i]=1
  }
}
Black_friday_data= cbind(Black_friday_data,Probability_level)
Black_friday_data$Probability_level=as.factor(Black_friday_data$Probability_level)
Black_friday_data$Gender = as.factor(Gender)
Black_friday_data$Occupation=as.factor(Occupation)
Black_friday_data$Marital_Status=as.factor(Marital_Status)

#11:
set.seed(9)


train_logistic=sample(1:537577,430061)
training_set_logistic= Black_friday_data[train_logistic,]
test_set_logistic=Black_friday_data[-train_logistic,]

library(MASS)
library(ISLR)
rm(Probability_level)
model_logistic<-glm(Probability_level~ Gender+Age+Occupation+ Purchase+City_Category+Stay_In_Current_City_Years+Marital_Status, data=Black_friday_data[train_logistic,], family="binomial")


#12:
model_logistic_prob=predict(model_logistic,newdata=test_set_logistic, type="response") #predict probabilities for all observations in the data
summary(model_logistic)
predicted_model_logistic<-ifelse(model_logistic_prob>0.7,1,0) # convert probabilities to class labels
#Threshold of 0.7 is selected because the predicted probabilities are very high and if we take 0.7 then only it starts classifying probabilities as 0 otherwise it classifies everything as 1. 
#Thus 0.7 is taken as threshold where it is fairly classifying 0 and 1.

table(test_set_logistic$Probability_level,predicted_model_logistic)
mean(test_set_logistic$Probability_level==predicted_model_logistic) #Accuracy
1-mean(test_set_logistic$Probability_level==predicted_model_logistic) #fraction of missclassified data
false_positive_rate=22435/107516
false_positive_rate
false_negative_rate=8394/107516
false_negative_rate

detach(Black_friday_data)
