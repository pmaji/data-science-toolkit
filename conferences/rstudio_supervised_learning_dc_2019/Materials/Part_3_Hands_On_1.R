# Slides for Applied Machine Learning workshop 

# Part_3_Feature_Engineering.R Hands-on break #1

# Slide 2 --------------------------------------------------------

library(tidymodels)
theme_set(theme_bw())

# ----------------------------------------------------------------

library(AmesHousing)
ames <- make_ames() %>%
  dplyr::select(-matches("Qu"))

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

set.seed(2453)
cv_splits <- vfold_cv(ames_train, v = 10, strata = "Sale_Price")

mod_rec <-
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood,
    data = ames_train
  ) %>%
  step_log(Sale_Price, base = 10) %>%
  step_other(Neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  prep(training = ames_train)

# ----------------------------------------------------------------

# Instead of using step_other(), take 10 minutes and research how
# to eliminate any zero-variance predictors using the recipe
# reference site.
#
# Re-run the recipe with this step.
#
# What were the results?
#
# Do you prefer either of these approaches to the other?
