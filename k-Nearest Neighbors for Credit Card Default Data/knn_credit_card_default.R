
# In this project we apply k-Nearest Neighbors algorithm to relatively large 
# dataset.

# Just as usual we start by importing the dataset and the libraries.

library(class)
library(readxl)

cc_default <- read_xls(path = "default of credit card clients.xls" , skip = 1)


# We shape the data as we need it.

cc_default <- cc_default[ , c("LIMIT_BAL",
                              "SEX",
                              "EDUCATION",
                              "MARRIAGE",
                              "AGE",
                              "PAY_0",
                              "PAY_2",
                              
                              
                              
                              
                              "BILL_AMT1",
                              "BILL_AMT2",
                              "BILL_AMT3",
                              
                              
                              
                              "PAY_AMT1",
                              "PAY_AMT2",
                              "PAY_AMT3",
                              
                              
                              
                              "default payment next month")]





cc_default$SEX <- as.factor(cc_default$SEX)
cc_default$EDUCATION <- as.factor(cc_default$EDUCATION)
cc_default$MARRIAGE <- as.factor(cc_default$MARRIAGE)
cc_default$AGE <- as.integer(cc_default$AGE)
cc_default$`default payment next month` <- as.factor(cc_default$`default payment next month`)


# OK. The data is ready. Let's separate the test set and the training set 
# and try the algorithm for k=3

test_index <- sample(x = 1:nrow(cc_default) , size = (nrow(cc_default)/10))

cc_default_training <- cc_default[-test_index , ]
cc_default_test <- cc_default[test_index , ]

cc_default_knn <-  knn(train = cc_default_training , test = cc_default_test ,
    cl = cc_default_training$`default payment next month` , k = 3 )


table(cc_default_knn , cc_default_test$`default payment next month`)
mean(cc_default_knn == cc_default_test$`default payment next month`)


# It worked. Now , to choose the optimum number of neighbors we run the loop
# below.

knn_accuracy <- vector(mode = "numeric" , length = 30)

for (i in 1:30) {
  
  test_index <- sample(x = 1:nrow(cc_default) , size = (nrow(cc_default)/10))
  
  cc_default_training <- cc_default[-test_index , ]
  cc_default_test <- cc_default[test_index , ]
  
  cc_default_knn <-  knn(train = cc_default_training , test = cc_default_test ,
                         cl = cc_default_training$`default payment next month` , k = 3 )
  
  
  
  knn_accuracy[i] <- mean(cc_default_knn == cc_default_test$`default payment next month`)
  

}

# The algorithm gives almost the same accuracy rate for every value of k. We can
# just go with 3 for the sake of simplicity.

