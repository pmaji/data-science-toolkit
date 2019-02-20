# Slides for Applied Machine Learning workshop 

# Part_2_Basic_Principles.R

# Slide 2 --------------------------------------------------------

library(tidymodels)
theme_set(theme_bw())

# Slide 11 -------------------------------------------------------

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
ames_test  <- testing(data_split)

nrow(ames_train)/nrow(ames)

# Slide 12 -------------------------------------------------------

# result of initial_split()
# <training / testing / total>
data_split

training(data_split)

# Slide 13 -------------------------------------------------------

ggplot(ames_train, aes(x = Sale_Price)) + 
  geom_line(stat = "density", trim = TRUE) + 
  geom_line(data = ames_test, 
            stat = "density", 
            trim = TRUE, col = "red") 

# Slide 15 -------------------------------------------------------

## model_fn(Sale_Price ~ Neighborhood + Year_Sold + Neighborhood:Year_Sold, data = ames_train)

## model_fn(Sale_Price ~ ., data = ames_train)

## model_fn(log10(Sale_Price) ~ ns(Longitude, df = 3) + ns(Latitude, df = 3), data = ames_train)

# Slide 17 -------------------------------------------------------

## # Usually, the variables must all be numeric
## pre_vars <- c("Year_Sold", "Longitude", "Latitude")
## model_fn(x = ames_train[, pre_vars],
##          y = ames_train$Sale_Price)

# Slide 18 -------------------------------------------------------

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

# Slide 21 -------------------------------------------------------

spec_lin_reg <- linear_reg()
spec_lin_reg

spec_lm <- set_engine(spec_lin_reg, "lm")
spec_lm

fit_lm <- fit(
  spec_lm,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

fit_lm

# Slide 22 -------------------------------------------------------

ames_train_log <- ames_train %>%
  mutate(Sale_Price_Log = log10(Sale_Price))

fit_xy(
  spec_lm,
  y = ames_train_log$Sale_Price_Log,
  x = ames_train_log[, c("Latitude", "Longitude")]
)

# Slide 23 -------------------------------------------------------

# optional:

library(rstanarm)
# reset the theme since rstanarm changes it :-(
theme_set(theme_bw())

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

coef(fit_stan$fit)

coef(fit_lm$fit)

# Slide 25 -------------------------------------------------------

summary(fit_lm$fit)

# Slide 34 -------------------------------------------------------

set.seed(2453)
cv_splits <- vfold_cv(
  data = ames_train, 
  v = 10, 
  strata = "Sale_Price"
)
cv_splits %>% slice(1:6)

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% analysis() %>% dim()
cv_splits$splits[[1]] %>% assessment() %>% dim()

# Slide 35 -------------------------------------------------------

geo_form <- log10(Sale_Price) ~ Latitude + Longitude

# Fit on a single analysis resample
fit_model <- function(split, spec) {
  fit(
    object = spec, 
    formula = geo_form,
    data = analysis(split) # <- pull out training set
  )
}

# For each resample, call fit_model()
cv_splits <- cv_splits %>% 
  mutate(models_lm = map(splits, fit_model, spec_lm))

cv_splits

# Slide 36 -------------------------------------------------------

compute_pred <- function(split, model) {
  
  # Extract the assessment set
  assess <- assessment(split) %>%
    mutate(Sale_Price_Log = log10(Sale_Price))
  
  # Compute predictions (a df is returned)
  pred <- predict(model, new_data = assess)
  
  bind_cols(assess, pred)
}

cv_splits <- cv_splits %>%
  mutate(pred_lm = map2(splits, models_lm, compute_pred))

cv_splits

# Slide 37 -------------------------------------------------------

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

cv_splits <- cv_splits %>%
  mutate(perf_lm = map(pred_lm, compute_perf))

cv_splits

# Slide 38 -------------------------------------------------------

cv_splits$perf_lm[[1]]

cv_splits %>%
  unnest(perf_lm) %>%
  group_by(.metric) %>%
  summarise(.estimate = mean(.estimate))

# Slide 40 -------------------------------------------------------

holdout_results <- 
  cv_splits %>%
  unnest(pred_lm) %>%
  mutate(.resid = Sale_Price_Log - .pred)

holdout_results %>% dim()
ames_train %>% dim()

# Slide 45 -------------------------------------------------------

spec_knn <- nearest_neighbor(neighbors = 2) %>%
  set_engine("kknn")

spec_knn

fit_knn <- fit(spec_knn, geo_form, ames_train_log)

fit_knn

# Slide 46 -------------------------------------------------------

# Predict on the same data you train with
repredicted <- fit_knn %>%
  predict(new_data = ames_train_log) %>%
  bind_cols(ames_train_log) %>%
  dplyr::select(.pred, Sale_Price_Log)

repredicted

# The ruckus is here!
repredicted %>% 
  rsq(
    truth = Sale_Price_Log, 
    estimate = .pred
  )

# Slide 47 -------------------------------------------------------

cv_splits <- cv_splits %>%
  mutate(
    # Fit a knn model for each split
    models_knn = map(splits, fit_model, spec_knn),
    
    # Generate predictions on the assessment set
    pred_knn = map2(splits, models_knn, compute_pred),
    
    # Calculation performance
    perf_knn = map(pred_knn, compute_perf)
  )


# Unnest & compute resampled performance estimates
cv_splits %>%
  unnest(perf_knn) %>%
  group_by(.metric) %>%
  summarise(
    .estimate_mean = mean(.estimate),
    .estimate_sd = sd(.estimate)
  )

# Slide 49 -------------------------------------------------------

extract_rmse <- function(perf_list) {
  perf_list %>%
    bind_rows() %>%
    filter(.metric == "rmse") %>%
    pull(.estimate)
}

rmse_lm <- extract_rmse(cv_splits$perf_lm)
rmse_knn <- extract_rmse(cv_splits$perf_knn)

rs_comp <- data.frame(
	rmse = c(rmse_lm, rmse_knn),
	Model = rep(c("Linear\nRegression", "2-NN"), each = nrow(cv_splits)),
	Resample = cv_splits$id
)

ggplot(rs_comp, aes(x = Model, y = rmse, group = Resample, col = Resample)) + 
  geom_point() + 
  geom_line() + 
  theme(legend.position = "none")

# Slide 50 -------------------------------------------------------

t.test(rmse_lm, rmse_knn, paired = TRUE)

# Slide 54 -------------------------------------------------------

#    ├── Create a set of candidate tuning parameter values
#    └── For each resample
#    │   ├── Split the data into analysis and assessment sets
#    │   ├── [preprocess data]
#    │   ├── For each tuning parameter value
#    │   │   ├── Fit the model using the analysis set
#    │   │   └── Compute the performance on the assessment set and save 
#    ├── For each tuning parameter value, average the performance over resamples
#    ├── Determine the best tuning parameter value
#    └── Create the final model with the optimal parameter(s) on the training set

# Slide 56 -------------------------------------------------------

# Parameter object for `neighbors`
neighbors

# Number of neighbors varies from 1-20
param_grid <- 
  neighbors %>% 
  range_set(c(1, 20)) %>%
  grid_regular(levels = 20)

glimpse(param_grid)

# Declare `neighbors` as varying
spec_knn_varying <- nearest_neighbor(
    neighbors = varying()
  ) %>%
  set_engine("kknn") %>% 
  set_mode("regression")  # not required

# Slide 57 -------------------------------------------------------

param_grid <- 
  param_grid %>%
  mutate(
    specs = merge(., spec_knn_varying)
  )

print(param_grid, n = 4)

param_grid$specs[[20]]

# Slide 58 -------------------------------------------------------

fit_one_spec_one_split <- function(spec, split) {
  mod <- fit_model(split, spec)
  pred_df <- compute_pred(split, mod)
  perf_df <- compute_perf(pred_df)
  
  # pull out only rmse
  perf_df %>%
    filter(.metric == "rmse") %>%
    pull(.estimate)
}

fit_one_spec_one_split(
  param_grid$specs[[6]],  # Six neighbors
  cv_splits$splits[[9]]  # Ninth Fold
)

# Slide 59 -------------------------------------------------------

fit_all_specs_one_split <- function(split, spec_df) {
  spec_df %>%
    mutate(
      rmse = map_dbl(
        specs, 
        fit_one_spec_one_split, 
        split = split
      )
    )
}

fit_all_specs_one_split(
  cv_splits$splits[[1]], 
  param_grid
) %>%
  print(n = 5)

# Slide 60 -------------------------------------------------------

fit_all_specs_all_splits <- function(split_df, spec_df) {
  split_df %>%
    mutate(
      spec_perf = map(
        splits, 
        fit_all_specs_one_split, 
        spec_df = spec_df
      )
    ) %>%
    dplyr::select(splits, id, spec_perf)
}

# Slide 61 -------------------------------------------------------

resampled_grid <- fit_all_specs_all_splits(
  split_df = cv_splits, 
  spec_df = param_grid
)

resampled_grid %>% slice(1:6)

# Keep the unnested version
unnested_grid <- 
  resampled_grid %>%
  unnest(spec_perf) %>%
  dplyr::select(-specs)

unnested_grid %>% slice(1:6)

# Slide 62 -------------------------------------------------------

rmse_by_neighbors <- 
  unnested_grid %>%
  group_by(neighbors) %>%
  summarize(rmse = mean(rmse))

ggplot(
    rmse_by_neighbors, 
    aes(x = neighbors, y = rmse)
  ) + 
  geom_point() + 
  geom_line()

# Slide 63 -------------------------------------------------------

best_neighbors <- 
  unnested_grid %>%
  group_by(id) %>%
  summarize(neighbors = neighbors[which.min(rmse)],
            rmse      = rmse[which.min(rmse)])

ggplot(rmse_by_neighbors, 
       aes(x = neighbors, y = rmse)) + 
  geom_point() + 
  geom_line() + 
  geom_line(data = unnested_grid, 
            aes(group = id, col = id),
            alpha = .2, lwd = 1) + 
  geom_point(data = best_neighbors, 
             aes(col = id),
             alpha = .5, cex = 2) +
  theme(legend.position = "none")

# Slide 64 -------------------------------------------------------

best_neighbor_value <-
  rmse_by_neighbors %>%
  filter(rmse == min(rmse)) %>%
  pull(neighbors)

best_spec <-
  param_grid %>%
  filter(neighbors == best_neighbor_value) %>%
  pull(specs) %>%
  .[[1]]

## fit(
##   best_spec,
##   geo_form,
##   ames_train
## )

