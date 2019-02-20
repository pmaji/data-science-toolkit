# Package installs -------------------------------------------------------------

# If you have any issues with the installs, please report them at:
# https://community.rstudio.com/t/information-for-the-2019-applied-machine-learning-workshop/19087/2

from_cran <- 
  c("AmesHousing", "broom", "caret", "dials", "doParallel", "e1071", "earth", 
    "ggrepel", "glmnet", "ipred", "klaR", "kknn", "pROC", "rpart", 
    "sessioninfo", "tidymodels")

install.packages(from_cran, repos = "http://cran.rstudio.com")

# check the installs:
for (pkg in from_cran)
  library(pkg, character.only = TRUE)

session_info()

if (!interactive())
  q("no")

