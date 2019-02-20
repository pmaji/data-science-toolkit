# Slides for Applied Machine Learning workshop 

# Part_2_Basic_Principles - Hands-On: Partial Residual Plots #2

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

# Start here from last hands-on session --------------------------

set.seed(2453)
cv_splits <- vfold_cv(
  data = ames_train, 
  v = 10, 
  strata = "Sale_Price"
)

# Functions ------------------------------------------------------

geo_form <- log10(Sale_Price) ~ Latitude + Longitude

fit_model <- function(split, spec) {
  fit(
    object = spec, 
    formula = geo_form,
    data = analysis(split) # <- pull out training set
  )
}

compute_pred <- function(split, model) {
  
  # Extract the assessment set
  assess <- assessment(split) %>%
    mutate(Sale_Price_Log = log10(Sale_Price))
  
  # Compute predictions (a df is returned)
  pred <- predict(model, new_data = assess)
  
  bind_cols(assess, pred)
}

compute_perf <- function(pred_df) {
  
  # Create a function that calculates
  # rmse and rsq and returns a data frame
  numeric_metrics <- metric_set(rmse, rsq)
  
  numeric_metrics(
    pred_df, 
    truth = Sale_Price_Log, 
    estimate = .pred
  )
}

# ----------------------------------------------------------------

spec_lm <- 
  linear_reg() %>% 
  set_engine(engine = "lm")

# ----------------------------------------------------------------

cv_splits <- 
  cv_splits %>% 
  mutate(
    models_lm = map(splits, fit_model, spec_lm), 
    pred_lm   = map2(splits, models_lm, compute_pred)
  ) 
  
# ----------------------------------------------------------------

holdout_results <- 
  cv_splits %>%
  unnest(pred_lm) %>%
  mutate(.resid = Sale_Price_Log - .pred)

# ----------------------------------------------------------------

#  A partial residual plot is used to diagnose what variables 
#  should have been in the model.
#  
#  We can plot the hold-out residuals versus different variables 
#  to understand if they should have been in the model
#  
#    - If the residuals have no pattern in the data, they are 
#      likely to be irrelevant.
#  
#    - If a pattern is seen, it suggests that the variable should 
#      have been in the model.
#  
#  Take 10 min and use `ggplot` to investigate the other predictors 
#  using the `holdout_results` data frame. `geom_smooth()` might 
#  come in handy.

