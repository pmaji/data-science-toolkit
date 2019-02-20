# Package installs -------------------------------------------------------------

from_cran <- 
  c("AmesHousing", "broom", "caret", "dials", "doParallel", "e1071", "earth", 
    "ggrepel", "glmnet", "ipred", "klaR", "kknn", "pROC", "rpart", 
    "sessioninfo", "tidymodels")

install.packages(from_cran, repos = "http://cran.rstudio.com")

# check the installs:
for (pkg in from_cran)
  library(pkg, character.only = TRUE)

sessioninfo::session_info()

if (!interactive())
  q("no")

