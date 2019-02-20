# Slides for Applied Machine Learning workshop 

# Part_5_Classification.R Hans-on #1

# ------------------------------------------------------------------------------

library(tidymodels)
thm <- theme_bw()
theme_set(thm)
library(caret)

# ------------------------------------------------------------------------------

load("Data/okc.RData")

# ------------------------------------------------------------------------------

ctrl <- trainControl(
  method = "cv",
  # Also predict the probabilities
  classProbs = TRUE,
  # Compute the ROC AUC as well as the sens and  
  # spec from the default 50% cutoff. The 
  # function `twoClassSummary` will produce those. 
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  sampling = "down"
)

# Previously... ----------------------------------------------------------------

# set.seed(5515)
# cart_mod <- train(
#   x = okc_train[, names(okc_train) != "Class"], 
#   y = okc_train$Class,
#   method = "rpart2",
#   metric = "ROC",
#   tuneGrid = data.frame(maxdepth = 1:20),
#   trControl = ctrl
# )


# Take the previous code and use the formula method to create the model. 
# For train(), the formula method always creates dummy variables for predictors 
# that are factors.
# 
# Is there any difference in performance?
#  
# Is the final model affected? How?
#  
# Take 15 min to answer these questions.



