
# In this project we apply Support Vector Machines model to a binary classification
# problem.

# AS always we begin with importing the libraries and the dataset.

library(e1071)

adult_income <- read.table(file = "adult.data" , stringsAsFactors = T)


# We shape and tame our data.

colnames(adult_income) <- c("age" ,
"workclass" ,
"fnlwgt" ,
"education" ,
"education-num" ,
"marital-status" ,
"occupation" ,
"relationship" ,
"race" ,
"sex" ,
"capital-gain" ,
"capital-loss" ,
"hours-per-week" ,
"native-country" , 
"income")


adult_income$age <- as.integer(adult_income$age)
adult_income$fnlwgt <- as.numeric(adult_income$fnlwgt)
adult_income$`capital-gain` <- as.numeric(adult_income$fnlwgt)
adult_income$`capital-loss` <- as.numeric(adult_income$fnlwgt)
adult_income$`hours-per-week` <- as.integer(adult_income$`hours-per-week`)

length(adult_income$age)
length(adult_income$workclass)
length(adult_income$income)


# The data is ready we set the test and training sets.

test_index <- sample(x = 1:nrow(adult_income) , size = (nrow(adult_income)%/%10))

adult_income_training <- adult_income[ -test_index , ]
adult_income_test <- adult_income[ test_index , ]
adult_income_test_target <- adult_income[ test_index , ]$income


# Now we apply the model to the data.

adult_income_svm <- svm(income ~ . ,
              data = adult_income_training ,
              kernel = 'radial' ,
              gamma = 1 , 
              cost = 1)

adult_income_predictions <- predict(object = adult_income_svm ,
                                    newdata = adult_income_test)

table(adult_income_predictions , adult_income_test_target)
table(adult_income_predictions == adult_income_test_target)
mean(adult_income_predictions == adult_income_test_target)

# Here we have a fair number of training errors. We can reduce training errors 
# by increasing cost parameter. However this comes with the risk of an irregular
# decision boundary and the risk of overfitting.


# We can perform cross-validation with tune() function and choose the best cost
# and gamma parameter. But since running these models take so much time even
# in modern computers I will tune my model in a smaller dataset.

tune.out <- tune(svm , income ~ . ,
                 data = adult_income[sample(x = 1:nrow(adult_income) ,
                                            size = (nrow(adult_income)%/%50)) , ] ,
                 kernel = 'radial' , 
                 ranges = list(
                   cost = c(seq.default(from = 0.1 , to = 2 , by = 0.1) , 
                            seq.default(from = 3 , to = 10 , by = 1)) , 
                   
                   gamma = c(seq.default(from = 0.1 , to = 2 , by = 0.1) , 
                             seq.default(from = 3 , to = 10 , by = 1))
                 ))

tune.out$best.parameters


# After havenig tuned the model we rerun the model with the best parameter.

adult_income_svm <- svm(income ~ . ,
                        data = adult_income_training ,
                        kernel = 'radial' ,
                        gamma = 0.1 , 
                        cost = 0.1)


adult_income_predictions <- predict(object = adult_income_svm ,
                                    newdata = adult_income_test)

table(adult_income_predictions , adult_income_test_target)
table(adult_income_predictions == adult_income_test_target)
mean(adult_income_predictions == adult_income_test_target)


# We have hot %80 accuracy rate. Not bad at all. With more detailed parameter 
# tuning we can get even better results.

