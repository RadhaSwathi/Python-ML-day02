#Naive Bayes Toy problem
#https://rpubs.com/dvorakt/144238
#all codes at the above link
#**********************One column*********************************
class=c("spam","ham","ham","ham")
lottery=c("Yes","No","No","Yes")
train=data.frame(class,lottery)
train
library(e1071)
mod=naiveBayes(class~lottery,data=train)#train the model
print(mod)

test=data.frame(lottery=c("Yes"))
test$lottery=factor(test$lottery,levels=c("No","Yes")) #train data had 2 levels. We have to make sure
#test also has the same levels
pred=predict(mod,test,type="raw")#make predictions
pred
#************************************2 columns**********************
train
train$reward=as.factor(c("Yes","Yes","Yes","No"))
train
str(train)
#add a column in the test data
test
test$reward=factor(c("Yes"),levels = c("No","Yes"))
str(test)
#build the model
mod2=naiveBayes(class~lottery+reward,data=train,type="raw")
print(mod2)
#make predictions
pred2=predict(mod2,newdata=test,type="raw")
pred2
#******************************************************
#CARS data from UCI
library(data.table)
df=fread("http://archive.ics.uci.edu/ml/machine-learning-databases/tic-tac-toe/tic-tac-toe.data")
#read the description about the data here
#http://archive.ics.uci.edu/ml/datasets/Tic-Tac-Toe+Endgame
head(df)
class(df)
str(df)
df=data.frame(sapply(df[,1:10],as.factor))# convert all variables to factors
str(df)
#split the data into 2 parts
library(caTools)
spl=sample.split(df,SplitRatio = 0.7)
train=subset(df,spl==T)
test=subset(df,spl==F)
library(e1071)
nb_model=naiveBayes(positive~.,data=train,type="raw")
#make a prediction on test data
nb_pred=predict(nb_model,newdata=test,type="raw")
nb_pred
#prepare the confusion matrix
table(test$positive,nb_pred[,2]>0.5)
accuracy=(43+159)/nrow(test)
accuracy
#accuracy of a baseline model
prop.table(table(test$positive))
