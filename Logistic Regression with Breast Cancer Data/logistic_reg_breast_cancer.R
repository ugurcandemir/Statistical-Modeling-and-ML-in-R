
# In this script we create a Logistic Regression model to predict the survival 
# rates of breast cancer patients.The dataset contains cases from a study that 
# was conducted between 1958 and 1970 at the University of Chicago's Billings 
# Hospital on the survival of patients who had undergone surgery for breast 
# cancer.

# Let's begin with importing the dataset to our R session.

breast_cancer <- read.table(file = "haberman.data" , sep = ",")


# Here we play with the data a little bit.

breast_cancer <- breast_cancer[ , -2]
colnames(breast_cancer) <- c('age' , 'nodes' , 'survival')
survival <- ifelse(test = breast_cancer$survival == 2 , yes = 0  , 1)
breast_cancer$survival <- survival


# The data is ready. We begin to analyze.

dim(breast_cancer)

test_index <- sample(x = 1:nrow(breast_cancer) , size = 61 )

breast_cancer_train <- breast_cancer[-test_index , ]
breast_cancer_test <- breast_cancer[test_index , ]
survival_test <- breast_cancer_test$survival

dim(breast_cancer_train)
dim(breast_cancer_test)


# We fit the model to our training data.

breast_cancer_logistic <- glm(data = breast_cancer_train ,
                              formula = survival ~ age + nodes ,
                              family = binomial)


summary(breast_cancer_logistic)
coef(breast_cancer_logistic)

# Age seems like an irrelevant variable. We use only the number of nodes as 
# predictor.

breast_cancer_logistic <- glm(data = breast_cancer_train ,
                              formula = survival ~  nodes ,
                              family = binomial)


summary(breast_cancer_logistic)
coef(breast_cancer_logistic)

# We obtain the fitted probabilities with predict() function.

breast_cancer_probs <- predict(object = breast_cancer_logistic ,
                               type = 'response' ,
                               newdata = breast_cancer_test)

# We choose the optimal threshold for Logistic Regression by running the loop
# below.

threshold_values <- seq.default(from = 0.05, to = 0.95 , by = 0.05)

for (each_threshold in threshold_values) {
  survival_pred <- rep(x = 0 , length(survival_test))
  survival_pred[breast_cancer_probs > each_threshold] <- 1
  print(paste( each_threshold , " : ",  mean(survival_pred == survival_test)))
  
  
}

# It seems like the maximum accuracy rate is between 0.6 and 0.65. We chooes 0.625.

survival_pred <- rep(x = 0 , length(survival_test))
survival_pred[breast_cancer_probs > 0.625] <- 1

table(survival_pred , survival_test)
mean(survival_pred == survival_test)

# We run the whole process 100 times to see the average accuracy rate of our model.

accuracy_rates <- vector(mode = "numeric" , length = 100)

for (i in 1:100) {
  
  test_index <- sample(x = 1:nrow(breast_cancer) , size = 61 )
  
  breast_cancer_train <- breast_cancer[-test_index , ]
  breast_cancer_test <- breast_cancer[test_index , ]
  survival_test <- breast_cancer_test$survival
  
  
  
  breast_cancer_logistic <- glm(data = breast_cancer_train ,
                                formula = survival ~  nodes ,
                                family = binomial)
  
  
  
  breast_cancer_probs <- predict(object = breast_cancer_logistic ,
                                 type = 'response' ,
                                 newdata = breast_cancer_test)
  
  
  
  survival_pred <- rep(x = 0 , length(survival_test))
  survival_pred[breast_cancer_probs > 0.625] <- 1
  
  accuracy_rates[i] <- mean(survival_pred == survival_test)

  
  
}


max(accuracy_rates)
min(accuracy_rates)
mean(accuracy_rates)

# It looks like our Logistic Regression Model predicts the survival status of a 
# breast cancer patient with %75.2 accuracy rate.

# If we wish to see the probability of survival for a particular node all we
# need to do is to use our model with predict() function by supplying the values
# to the newdata parameter.


breast_cancer_probs <- predict(object = breast_cancer_logistic , 
                               newdata = data.frame(nodes = c(5 , 10 , 15)) ,
                               type = 'response')
breast_cancer_probs


