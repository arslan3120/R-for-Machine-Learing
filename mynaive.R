Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
 
# Loading package
library(e1071)
library(caTools)
library(caret)
################Data description###################################
data (iris)
print(iris)
summary(iris)
#ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species))+geom_point()
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species))+geom_point() +geom_smooth()

#######################################3
# Splitting data into train and test data
#######################################
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

##################################################

# creating training data as 80% of the dataset
random_sample <- createDataPartition(iris$Species, 
                               p = 0.6, list = FALSE)
print(random_sample) 

# generating training dataset
# from the random_sample

training_dataset  <- iris[random_sample, ]
print(training_dataset) 
# generating testing dataset
# from rows which are not 
# included in random_sample
testing_dataset <- iris[-random_sample, ]
print(testing_dataset)

###################################################
# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])
 
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Species ~ ., data = train_scale)
classifier_cl
 
# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_scale)
 
# Confusion Matrix
cm <- table(test_cl$Species, y_pred)
cm
 
# Model Evaluation
aa=confusionMatrix(cm)
misclassification=1-aa$Accuracy
