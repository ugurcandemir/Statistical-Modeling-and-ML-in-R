

# In this script we develop a Linear Discriminant Analysis model to predict if
# a real estate is "Basic" or "Luxury". The data is from Kaggle.

# As usual we start with importing the libraries and the dataset.

library(MASS)

paris <- read.csv(file = "ParisHousingClass.csv")


# We do a a bit of data cleaning.

paris <- paris[ , c("squareMeters" , "hasPool" , "isNewBuilt" , "price" ,
                    "category")]

paris$hasPool <- as.factor(paris$hasPool)
paris$isNewBuilt <- as.factor(paris$isNewBuilt)
paris$category <- as.factor(paris$category)

str(paris)


# The data is ready. Now we divide it into test and training sets.

test_index <- sample(x = 1:nrow(paris) , size = (nrow(paris)%/%10))

paris_training <- paris[ - test_index , ]
paris_test <- paris[ test_index , ]
paris_test_target <- paris_test$category

dim(paris_test)
dim(paris_training)
length(paris_test_target)


# Here we use MASS::lda() to fit Linear Discriminant model.

paris_lda <- lda(category ~ squareMeters + hasPool + isNewBuilt + price , 
                 data = paris_training )


paris_lda_predictions <- predict(object = paris_lda , newdata = paris_test)


mean(paris_lda_predictions$class == paris_test_target)
table(paris_lda_predictions$class , paris_test_target)


# Our data predicts if a real estate is "Basic" or "Luxury" with %93 accuracy.
 