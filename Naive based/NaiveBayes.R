df=read.csv("stevens.csv")
df$Reverse=as.factor(df$Reverse)
head(df)
library(e1071)
train=head(df,7)
test=tail(df,1)
mod=naiveBayes(Reverse~LowerCourt,data=train)
mod
summary(mod)
print(mod)
pred=predict(mod,newdata = test[,3],type="raw")
pred
