# Slides for Applied Machine Learning workshop 

# Part_2_Basic_Principles - Hands-On: Some Basic Diagnostics #1

# ----------------------------------------------------------------

library(tidymodels)

# ----------------------------------------------------------------

library(AmesHousing)
ames <-
  make_ames() %>%
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
nrow(ames)

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)

# ----------------------------------------------------------------

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
simple_lm_values <- augment(simple_lm)

# ----------------------------------------------------------------

# From these results, let's take 10 minutes and do some visualizations:
#
#  - Plot the observed versus fitted values
#
#  - Plot the residuals
#
#  - Plot the predicted versus residuals
#
# Are there any downsides to this approach?
