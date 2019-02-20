# Slides for Applied Machine Learning workshop 

# Part_5_Classification.R Hans-on #2

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
# cart_bag <- train(
#   x = okc_train[, names(okc_train) != "Class"], 
#   y = okc_train$Class,
#   method = "treebag",
#   metric = "ROC",
#   trControl = ctrl
# )

# As previously mentioned, caret uses ipred::bagging() to create the model.
# 
# Look at the help function to determine which bagging() argument controls the 
# number of bootstraps.
# 
# How can we make train() use a different value? (hint: ?train)
# 
# Does changing this affect the area under the ROC curve?
#   
# Take another 10 mins.

