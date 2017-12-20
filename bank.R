bank=read.csv("F:/kaggle/bankbalanced/bank.csv")
View(bank) 
str(bank)

#number of numeric variables
x<-sapply(bank,is.numeric)
bank_numeric<-bank[,x]
head(bank_numeric)
#7 numeric variables

#number of factor variables
y<-sapply(bank, is.factor)
bank_factor<-bank[,y]
head(bank_factor)
#10 factor variables

#missing values treatment
library(Amelia)
missmap(bank, y.at = 1,y.labels = "",col=c("red","black"),legend = FALSE)
sum(is.na(bank))
#no missing values

#feature engineering

#age outliers
boxplot(bank$age)

#age outlier capping
upper_side_outliers_age <- quantile(bank$age, 0.75) + 1.5*IQR(bank$age)
bank[bank$age > round(upper_side_outliers_age), "age"] <- round(upper_side_outliers_age)
min(bank$age)
max(bank$age)
bank$age<-ifelse(bank$age<=18 & bank$age<40,"adult",ifelse(bank$age>=40 & bank$age<58,
          "middle age","older"))
bank$age<-as.factor(bank$age)
View(bank$age)

#duration outliers
bank$duration<-(bank$duration/60)
View(bank$duration)
boxplot(bank$duration)

#duration outlier capping
upper_side_outliers_duration <- quantile(bank$duration, 0.75) + 1.5*IQR(bank$duration)
bank[bank$duration > round(upper_side_outliers_duration), "duration"] <- round(upper_side_outliers_duration)
min(bank$duration)
max(bank$duration)
boxplot(bank$duration)
bank$duration<-ifelse(bank$duration<5 ,"1",ifelse(bank$duration>=5 & bank$duration<10,"2",
               ifelse(bank$duration>=10 & bank$duration<15,"3","4")))
bank$duration<-as.factor(bank$duration)

#EDA
library(ggplot2)
ggplot(bank,aes(age,fill=deposit))+geom_bar()+
  ggtitle("Age vs Deposit")->p1
p1
#older people have deposit rate

ggplot(bank,aes(job,fill=deposit))+geom_bar()+
  ggtitle("Job vs Deposit")->p2
p2
#people who are at the management deposit more followed by technicians and bluecollars

ggplot(bank,aes(marital,fill=deposit))+geom_bar()+
  ggtitle("Marital vs Depsoit")->p3
p3
#married people deposit more than others

ggplot(bank,aes(education,fill=deposit))+geom_bar()+
  ggtitle("Education vs Deposit")->p4
p4
#people who have secondary level education deposit more followed by 
#people who have tertiary level education

ggplot(bank,aes(default,fill=deposit))+geom_bar()+
  ggtitle("Default vs Deposit")->p5
p5
#people who have no credit default deposit more and people who have credit default
#almost have no deposit

ggplot(bank,aes(balance))+geom_histogram(aes(fill=deposit),color="black")+
  ggtitle("Balance vs Deposit")->p6
p6
#people who have below 500 deposit more

ggplot(bank,aes(housing,fill=deposit))+geom_bar()+
  ggtitle("Housing vs Deposit")->p7
p7
#people who dont have house loan deposit more than people who have home loans.

ggplot(bank,aes(loan,fill=deposit))+geom_bar()+
  ggtitle("Loan vs Deposit")->p8
p8
#people who dont have personal loan deposit more than people who have personal loans

ggplot(bank,aes(contact,fill=deposit))+geom_bar()+
  ggtitle("Contact vs Deposit")->p9
p9
#people who are contacted by cellular deposit more

ggplot(bank,aes(month,fill=deposit))+geom_bar()+
  ggtitle("Month vs Deposit")->p10
p10
#people who are contacted in the month of may deposit more followed by august,july and april

ggplot(bank,aes(campaign))+geom_histogram(aes(fill=deposit),color="black",binwidth =5)+
  ggtitle("Campaign vs Deposit")->p11
p11
#people who are contacted for less than 5 times deposit more

ggplot(bank,aes(duration,fill=deposit))+geom_bar()+
  ggtitle("Duration vs Deposit")->p12
p12
#people whose duration of contact is less than 5 minutes deposit more 
#but people whose duration of contact exceeded 5 minutes have higher deposit rate


library(gridExtra)
grid.arrange(p1,p2,p3)->g1
g1
grid.arrange(p4,p5,p6)->g2
g2
grid.arrange(p7,p8,p9)->g3
g3
grid.arrange(p10,p11,p12)->g4
g4

#correaltion matrix
library(corrplot)
library(psych)
bank_cor <- bank

for(i in 1:ncol(bank_cor)){
  
  bank_cor[,i]<- as.integer(bank_cor[,i])
}

corrplot(cor(bank_cor))
# pdays, previous and poutcome are highly correlated variables

#outliers
boxplot(bank$balance)
upper_side_outliers_balance <- quantile(bank$balance, 0.75) + 1.5*IQR(bank$balance)

#Capping was done on this part
bank[bank$balance > round(upper_side_outliers_balance), "balance"] <- round(upper_side_outliers_balance)

#base accuracy
prop.table(table(bank$deposit))
#47 percent accuracy

bank$deposit<-ifelse(bank$deposit=="yes",1,0)

#splitting the data
library(caTools)
set.seed(1234)
split <- sample.split(bank$deposit, SplitRatio = 0.7)
train <- subset(bank, split == TRUE)
test <- subset(bank, split == FALSE)

#model bulding without removing correlated variables
log.model1 <- glm(deposit ~ ., data=train, family = binomial(link='logit'))
summary(log.model1)
# aic value 6795.9

## coefficients
exp(coef(log.model1))

#beta coefficients of model with confidence interval
exp(cbind(OR = coef(log.model1), confint(log.model1)))

#Validating model 1

#prediction
pred1 <- predict(log.model1, newdata=test, type = "response")
table(test$deposit, pred1>= 0.5)
(1513+1219)/nrow(test)
#81 percent accuracy


#confusion matrix
library(caret)
pred_threshold1<-ifelse(pred1>=0.5,1,0)
confusionMatrix(pred_threshold1,test$deposit)

# Area under the curve
library(ROCR)
predic1<-prediction(pred1, test$deposit)

# creating ROC curve
roc1<-performance(predic1,"tpr","fpr")
plot(roc1)
title("ROC Curve")
auc1<- performance(predic1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1
#auc value 0.89

#cross validation
ctrl1 <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit1 <- train(deposit ~.,  data=bank, method="glm", family="binomial",
                 trControl = ctrl1, tuneLength = 5)
summary(mod_fit1)

pred_cross1 = predict(mod_fit1, newdata=test)
pred_cross1<-ifelse(pred_cross1>=0.5, 1,0)
confusionMatrix(pred_cross1, test$deposit)
#accuracy 81

#variable importance
library(caret)
varImp(log.model1,scale=FALSE)

#building model without correlated variables
log.model2<-glm(deposit~age+job+marital+education+default+balance+housing+ 
                  loan+contact+day+ month + duration + campaign,   
                data=train, family = binomial(link = 'logit'))

summary(log.model2)
# aic value 7271.2

## coefficients
exp(coef(log.model2))

#beta coefficients of model with confidence interval
exp(cbind(OR = coef(log.model2), confint(log.model2)))

#Validating model 2

#prediction
pred2 <- predict(log.model2, newdata=test, type = "response")
table(test$deposit, pred2 >= 0.5)
(1474+1223)/nrow(test)
#80 percent accuracy

#confusion matrix
pred_threshold2<-ifelse(pred2>=0.5,1,0)
confusionMatrix(pred_threshold2,test$deposit)

# Area under the curve
library(ROCR)
predic2<-prediction(pred2, test$deposit)

# creating ROC curve
roc2<-performance(predic2,"tpr","fpr")
plot(roc2)
title("ROC Curve")
auc2 <- performance(predic2, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2
#auc value 0.88

#cross validation
ctrl2 <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit2 <- train(deposit~age+job+education+default+balance+housing+ 
                  loan+contact+day+ month + duration + campaign,  data=bank, method="glm", family="binomial",
                  trControl = ctrl2, tuneLength = 5)
summary(mod_fit2)

pred_cross2 = predict(mod_fit2, newdata=test)
pred_cross2<-ifelse(pred_cross2>=0.5, 1,0)
confusionMatrix(pred_cross2, test$deposit)
#accuracy 80

#variable importance
varImp(log.model2,scale=FALSE)

#model1 has more acccuracy  than model2
#model 1 has has higher auc vlaue than model 2
#model 1 has more accuracy than model 2 in cross validation
#model 1 has lower aic value than model 2 in cross validation
#so model 1 is better than model 2.
