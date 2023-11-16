install.packages("caTools")
install.packages("dplyr")     # for data wrangling
install.packages("rsample")
install.packages("caret")

library(caTools)
library(dplyr)     # for data wrangling
library(rsample)   # for data splitting
library(caret)     # for modelling/logistic regression modeling

###############data

aa=read.csv("C:/Users/Administrator/Desktop/smoke3.csv")
pp<- c(0.7,0.75,0.8)

for (x in pp) {
  
  # Using rsample package
  split_1  <- initial_split(aa, prop = x)
  train2  <- training(split_1)
  test2   <- testing(split_1)
  head(aa)
  glm_out <- glm(
    Fire.Alarm ~.,
    data = train2,
    family = binomial
  ) # family = binomial required for logistic regression
  summary(glm_out)
  yresponse<-predict(glm_out,test2,type='response')
  thres1<-ifelse(yresponse>0.7,1,0) 
  cm=table(test2$Fire.Alarm,thres1)
  
  accuracy2 = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  err=1-accuracy2
  ##########other way######################
  accuracy1=sum(diag(cm))/sum(cm)
  print(accuracy1)
}


specificity(cm)
sensitivity(cm)
confusionMatrix(cm)
spec = (cm[1,1]) / (cm[1,1] + cm[2,1])
spec
sens = (cm[2,2] / (cm[2,2] + cm[1,2]))
sens
