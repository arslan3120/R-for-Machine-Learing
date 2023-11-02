#Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages("caret")
install.packages("installr")
install.packages("")
install.packages("class")




 
# Loading package
library(e1071)
library(caTools)
library(class)
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
classifier_cl <- naiveBayes(Species ~ ., data = train_cl)
classifier_cl
 
# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)
 
# Confusion Matrix
cm <- table(test_cl$Species, y_pred)
cm
 
# Model Evaluation
aa = confusionMatrix(cm)
misclassification=1-aa$Accuracy
misclassification
accuracy = aa$overall['Accuracy']
accuracy



sensitivity_value <- cm["setosa", "setosa"] / sum(cm["setosa", ])

specificity_value <- sum(cm[c("versicolor", "virginica"), c("versicolor", "virginica")]) / 
  sum(cm[c("versicolor", "virginica"), ])

cat("Sensitivity (True Positive Rate): ", sensitivity_value, "\n")
cat("Specificity (True Negative Rate): ", specificity_value, "\n")



diagonal_sum1 <- sum(diag(cm))
diagonal_sum1
total_sum <- sum(cm)
accuracy1 <- diagonal_sum1 / total_sum
accuracy1








# For Knn

class_knn = knn(train=train_scale,test=test_scale,cl=train_cl$Species,k=3)
class_knn

# Calculate the confusion matrix
cm <- confusionMatrix(data = class_knn, reference = actual_labels)

# Print confusion matrix and accuracy
print(cm)
cat("Accuracy:", cm$overall[1], "\n")
