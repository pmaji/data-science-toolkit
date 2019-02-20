# Slides for Applied Machine Learning workshop 

# Part_4_Regression_Modeling.R

# Slide 3 --------------------------------------------------------

library(tidymodels)
library(ggrepel)
theme_set(theme_bw())

# Slide 5 --------------------------------------------------------

url <- "https://github.com/topepo/cars/raw/master/2018_12_02_city/car_data_splits.RData"
temp_save <- tempfile()
download.file(url, destfile = temp_save)
load(temp_save)

car_train %>% bind_rows(car_test) %>% group_by(year) %>% count()

# Slide 6 --------------------------------------------------------

removals <- c("CNG", "Electricity")

car_train <- 
  car_train %>% 
  dplyr::filter(!(fuel_type %in% removals)) %>%
  mutate(fuel_type = relevel(fuel_type, "Gasoline_or_natural_gas"))

car_test <-
  car_test %>% 
  dplyr::filter(!(fuel_type %in% removals)) %>%
  mutate(fuel_type = relevel(fuel_type, "Gasoline_or_natural_gas"))

# Slide 9 --------------------------------------------------------

## library(splines)
## lm(mpg ~ . -model + ns(eng_displ, 4) + ns(cylinders, 4), data = car_train)

# Slide 10 -------------------------------------------------------

car_train %>%
  group_by(make) %>%
  count() %>%
  arrange(n) %>%
  head(6)

# Slide 10 -------------------------------------------------------

basic_rec <- recipe(mpg ~ ., data = car_train) %>%
  # keep the car name but don't use as a predictor
  update_role(model, new_role = "model") %>%
  # collapse some makes into "other"
  step_other(make, car_class, threshold = 0.005) %>%
  step_other(fuel_type, threshold = 0.01) %>%
  step_dummy(all_nominal(), -model) %>%
  step_zv(all_predictors())
# Slide 15 -------------------------------------------------------

glmn_grid <- expand.grid(alpha = seq(0, 1, by = .25), 
                         lambda = 10^seq(-3, -1, length = 20))
nrow(glmn_grid)

# Slide 18 -------------------------------------------------------

library(caret)
ctrl <- trainControl(
  method = "cv", 
  # Save the assessment predictions from the best model
  savePredictions = "final",
  # Log the progress of the tuning process
  verboseIter = TRUE
  )

# Slide 19 -------------------------------------------------------

glmn_rec <- 
  basic_rec %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_ns(eng_displ, cylinders, options = list(df = 4))

set.seed(92598)
glmn_mod <- train(
  glmn_rec, 
  data = car_train,
  method = "glmnet", 
  trControl = ctrl,
  tuneGrid = glmn_grid
  )

# Slide 21 -------------------------------------------------------

glmn_mod$bestTune

ggplot(glmn_mod) + scale_x_log10() + theme(legend.position = "top")

# Slide 22 -------------------------------------------------------

glmn_mod$pred %>% head(4)

ggplot(glmn_mod$pred, aes(x = obs, y = pred)) +
  geom_abline(col = "green", alpha = .5) + 
  geom_point(alpha = .3) + 
  geom_smooth(se = FALSE, col = "red", 
              lty = 2, lwd = 1, alpha = .5)


# Slide 23 -------------------------------------------------------

add_columns <- function(x, dat, ...) {
  # capture any selectors and filter the data
  dots <- quos(...)
  if (!is_empty(dots))
    dat <- dplyr::select(dat, year, model, !!!dots)
  
  dat <-
    x %>%
    pluck("pred") %>%
    arrange(rowIndex) %>%
    dplyr::select(-rowIndex) %>%
    bind_cols(dat)
  
  # create a label column when possible
  if (all(c("model", "year") %in% names(dat)))
    dat <-
    dat %>%
    mutate(plot_label = paste(year, model))
  dat
}

# Slide 24 -------------------------------------------------------

obs_pred_plot <- function(x, dat, cutoff = 25, ...) {
  
  pred_dat <- x %>%
    add_columns(dat, model, year) %>%
    mutate(residuals = obs - pred) 
  
  ggplot(pred_dat, aes(x = pred, y = obs)) +
    
    geom_abline(col = "green", alpha = .5) + 
    
    geom_point(alpha = .3) + 
    
    geom_smooth(
      se = FALSE, col = "red", 
      lty = 2, lwd = .25, alpha = .5
    ) + 
    
    geom_text_repel(
      data = dplyr::filter(pred_dat, abs(residuals) > cutoff),
      aes(label = plot_label),
      segment.color = "grey50"
    )
}

# Slide 24 -------------------------------------------------------

resid_plot <- function(x, dat, cutoff = 25, ...) {
  
  pred_dat <- x %>%
    add_columns(dat, model, year) %>%
    mutate(residuals = obs - pred) 
  
  ggplot(pred_dat, aes(x = pred, y = residuals)) +
    
    geom_hline(col = "green", yintercept = 0) + 
    
    geom_point(alpha = .3) + 
    
    geom_text_repel(
      data = dplyr::filter(
        pred_dat, 
        abs(residuals) > cutoff
      ),
      aes(label = plot_label),
      segment.color = "grey50"
    )
}

# Slide 25 -------------------------------------------------------

obs_pred_plot(glmn_mod, car_train)

resid_plot(glmn_mod, car_train)

# Slide 26 -------------------------------------------------------

reg_imp <- varImp(glmn_mod, scale = FALSE)
ggplot(reg_imp, top = 30) + xlab("")

# Slide 28 -------------------------------------------------------
## library(glmnet)
## plot(glmn_mod$finalModel, xvar = "lambda")

# Slide 29/30 ----------------------------------------------------

# Get the set of coefficients across penalty values
tidy_coefs <- broom::tidy(glmn_mod$finalModel) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::select(-step, -dev.ratio)

# Get the lambda closest to caret's optimal choice
delta <- abs(tidy_coefs$lambda - glmn_mod$bestTune$lambda)
lambda_opt <- tidy_coefs$lambda[which.min(delta)]

# Keep the large values
label_coefs <- tidy_coefs %>%
  mutate(abs_estimate = abs(estimate)) %>%
  dplyr::filter(abs_estimate >= 3) %>%
  distinct(term) %>%
  inner_join(tidy_coefs, by = "term") %>%
  dplyr::filter(lambda == lambda_opt)

# plot the paths and highlight the large values
tidy_coefs %>%
  ggplot(aes(x = lambda, y = estimate, group = term, col = term, label = term)) +
  geom_line(alpha = .4) +
  theme(legend.position = "none") +
  scale_x_log10() +
  geom_text_repel(data = label_coefs, aes(x = .0005))

# Slide 43 -------------------------------------------------------

ctrl$verboseIter <- FALSE

mars_grid <- expand.grid(degree = 1:2, nprune = seq(2, 26, by = 2))

# Using the same seed to obtain the same 
# resamples as the glmnet model.
set.seed(92598)
mars_mod <- train(
  basic_rec, 
  data = car_train,
  method = "earth",
  tuneGrid = mars_grid,
  trControl = ctrl
)

# Slide 46 -------------------------------------------------------

ggplot(mars_mod) + theme(legend.position = "top")

# Slide 47 -------------------------------------------------------

obs_pred_plot(mars_mod, car_train)

resid_plot(mars_mod, car_train)

# Slide 48 -------------------------------------------------------

library(earth)
mars_mod$finalModel

# Slide 49 -------------------------------------------------------

mars_mod$finalModel %>% format() %>% cat()

# Slide 50 -------------------------------------------------------

mars_imp <- varImp(mars_mod)
ggplot(mars_imp, top = 20) + xlab("")

# Slide 51 -------------------------------------------------------

set.seed(92598)
mars_gcv_mod <- train(
  basic_rec, 
  data = car_train,
  method = "gcvEarth",
  tuneGrid = data.frame(degree = 1:2),
  trControl = ctrl
)
mars_gcv_mod$finalModel

# Slide 52 -------------------------------------------------------

set.seed(92598)
mars_gcv_bag <- train(
  basic_rec, 
  data = car_train,
  method = "bagEarthGCV",
  tuneGrid = data.frame(degree = 1:2),
  trControl = ctrl,
  # Number of bootstraps for `bagEarth` function
  B = 50
)

# Slide 57 -------------------------------------------------------

mars_gcv_bag

# Slide 58 -------------------------------------------------------

obs_pred_plot(mars_gcv_bag, car_train)

resid_plot(mars_gcv_bag, car_train)

# Slide 59 -------------------------------------------------------

compare_models(mars_gcv_bag, mars_mod)

# Slide 61 -------------------------------------------------------

car_test <- car_test %>%
  mutate(pred = predict(mars_gcv_bag, car_test))

rmse(car_test, truth = mpg, estimate = pred)

ggplot(car_test, aes(x = mpg, y = pred, label = model)) +
  geom_abline(col = "green", alpha = .5) +
  geom_point(alpha = .3) +
  geom_smooth(se = FALSE, col = "red",
              lty = 2, lwd = .5, alpha = .5) +
  geom_text_repel(
    data = car_test  %>% dplyr::filter(abs(mpg - pred) > 10),
    segment.color = "grey50"
    )
