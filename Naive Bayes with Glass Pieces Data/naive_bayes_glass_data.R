
# In this project we apply Naive Bayes algorithm for classification of glass
# pieces. The study of classification of types of glass was motivated by 
# criminological investigation. At the scene of the crime, the glass left can be
# used as evidence , if it is correctly identified!

# Just as always we start by importing the dataset and the libraries.

library(e1071)

glass <- read.table(file = "glass.data" , sep = ",")



# We shape and clean our data.

column_names <-c(
"id",
"refractive index",
"Sodium" ,
"Magnesium",
"Aluminum",
"Silicon",
"Potassium",
"Calcium",
"Barium",
"Iron",
"Type_of_glass" )

colnames(glass) <- column_names

glass <- glass[ , -1]

glass$Type_of_glass <- as.factor(glass$Type_of_glass )


# It's done. Now we move to the next stage and separate test and training sets.

test_index <- sample(x = 1:nrow(glass) , size = (nrow(glass)%/%10))

glass_training <- glass[ -test_index , ]
glass_test <- glass[ test_index , ]
glass_test_target <- glass[ test_index , ]$Type_of_glass

str(glass_test)
str(glass_training)
length(glass_test_target)


# Now we apply the algorithm.

glass_naive_bayes <- naiveBayes(Type_of_glass ~ . , data = glass)

glass_naive_bayes_predictions <- predict(object = glass_naive_bayes ,
                                         newdata = glass_test  )

table(glass_naive_bayes_predictions , glass_test_target)

mean(glass_naive_bayes_predictions == glass_test_target)

# After having run the script several times we conclude that the accuracy rate
# is %47. At first it may seem low. But regarding that there are seven classes
# the random guess accuracy rate is approximately %14. Given the relatively
# small size of the dataset I would confidently argue that our algorithm performed
# quite well.
