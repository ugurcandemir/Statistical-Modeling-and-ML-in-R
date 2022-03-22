
# In this project we apply Random Forest and Bootstrap Aggregating(Bagging)
# algorithms to a heart disease data.

# We start by importing the dataset and the libraries.

library(randomForest)

heart_disease <- read.csv(file = "heart_2020_cleaned.csv" , stringsAsFactors = T)

str(heart_disease)
head(heart_disease)
tail(heart_disease)


# The dataset is all clean. We divide it into test and training parts.

test_index <- sample(x = 1:nrow(heart_disease) , size = (nrow(heart_disease)%/%10))
length(test_index)

heart_disease_training <- heart_disease[ - test_index , - 1]
heart_disease_training_target <- heart_disease[ - test_index , 1]
heart_disease_test <- heart_disease[ test_index , -1]   
heart_disease_test_target <- heart_disease[ test_index , ]$HeartDisease


str(heart_disease_training)
str(heart_disease_training_target)
str(heart_disease_test)
str(heart_disease_test_target)


# The dataset is ready for analysis. Let's apply our algorithms.
# Random Forest is a special case of Bagging where we don't count all the 
# variables for tree selection but only some of them.

heart_disease_bagging <- randomForest(x = heart_disease_training ,
                                      y = heart_disease_training_target ,
                                      xtest = heart_disease_test ,
                                      ytest = heart_disease_test_target ,
                                      mtry = 17)

table(heart_disease_bagging$test$predicted , heart_disease_target)

mean(heart_disease_bagging$test$predicted == heart_disease_target)

# The result is not bad at all. Let's see what Random Forest will do.
# By default the randomForest::randomForest() takes square root of the number of
# total variables for tree selection. In this case it is 4.

heart_disease_random_forest <- randomForest(x = heart_disease_training ,
                                            y = heart_disease_training_target ,
                                            xtest = heart_disease_test )




table(heart_disease_random_forest$test$predicted , heart_disease_target)

mean(heart_disease_random_forest$test$predicted == heart_disease_target)

# But of the algorithms have performed very well in this dataset. The relatively
# large data size has also helped.

