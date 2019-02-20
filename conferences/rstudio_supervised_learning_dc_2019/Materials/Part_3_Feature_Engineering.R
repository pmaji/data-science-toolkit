# Slides for Applied Machine Learning workshop 

# Part_3_Feature_Engineering.R

# Slide 2 --------------------------------------------------------

library(tidymodels)
theme_set(theme_bw())

# Slide 3 --------------------------------------------------------

# Previously...

library(AmesHousing)
ames <- make_ames() %>% 
  dplyr::select(-matches("Qu"))

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

set.seed(2453)
cv_splits <- vfold_cv(ames_train, v = 10, strata = "Sale_Price")

# Slide 12 -------------------------------------------------------

mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10)

# Slide 13 -------------------------------------------------------

mod_rec <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

mod_rec

# Slide 15 -------------------------------------------------------

mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE)

# Slide 16 -------------------------------------------------------

mod_rec_trained

# Slide 17 -------------------------------------------------------

ames_test_dummies <- bake(mod_rec_trained, new_data = ames_test)
names(ames_test_dummies)

# Make the example data using data from caret --------------------

library(caret)
data(segmentationData)

segmentationData <- 
  segmentationData %>% 
  dplyr::select(EqSphereAreaCh1, PerimCh1, Class, Case) %>% 
  setNames(c("PredictorA", "PredictorB", "Class", "Case")) %>% 
  mutate(Class = factor(ifelse(Class == "PS", "One", "Two")))

bivariate_data_train <- 
  segmentationData %>% 
  dplyr::filter(Case == "Train") %>% 
  dplyr::select(-Case)

bivariate_data_test  <- 
  segmentationData %>% 
  dplyr::filter(Case == "Test") %>% 
  dplyr::select(-Case)

# Slide 21 -------------------------------------------------------

ggplot(bivariate_data_test, 
       aes(x = PredictorA, 
           y = PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top")

# Slide 22/23 ----------------------------------------------------

bivariate_rec <- 
  recipe(Class ~ PredictorA + PredictorB, data = bivariate_data_train) %>%
  step_BoxCox(all_predictors()) %>% 
  prep(training = bivariate_data_train)

inverse_test <- bake(bivariate_rec, new_data = bivariate_data_test, everything())

ggplot(inverse_test, 
       aes(x = 1/PredictorA, 
           y = 1/PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  xlab("1/A") + ylab("1/B") 


# Slide 28/29 ----------------------------------------------------

bivariate_pca <- 
  recipe(Class ~ PredictorA + PredictorB, data = bivariate_data_train) %>%
  step_BoxCox(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors()) %>%
  prep(training = bivariate_data_test, verbose = FALSE)

pca_test <- bake(bivariate_pca, new_data = bivariate_data_test)

# Put components axes on the same range
pca_rng <- extendrange(c(pca_test$PC1, pca_test$PC2))

ggplot(pca_test, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(alpha = .2, cex = 1.5) + 
  theme(legend.position = "top") +
  xlim(pca_rng) + ylim(pca_rng) + 
  xlab("Principal Component 1") + ylab("Principal Component 2") 

# Slide 32 -------------------------------------------------------

price_breaks <- (1:6)*(10^5)

ggplot(
    ames_train, 
    aes(x = Year_Built, y = Sale_Price)
  ) + 
  geom_point(alpha = 0.4) +
  scale_x_log10() + 
  scale_y_continuous(
    breaks = price_breaks, 
    trans = "log10"
  ) +
  geom_smooth(method = "loess")

# Slide 33 -------------------------------------------------------

library(MASS) # to get robust linear regression model

ggplot(
    ames_train, 
    aes(x = Year_Built, 
        y = Sale_Price)
  ) + 
  geom_point(alpha = 0.4) +
  scale_y_continuous(
    breaks = price_breaks, 
    trans = "log10"
  ) + 
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm") 

# Slide 34 -------------------------------------------------------

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air, data = ames_train)
mod2 <- lm(
  log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built:Central_Air, 
  data = ames_train
)
anova(mod1, mod2)

# Slide 35 -------------------------------------------------------

recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  prep(training = ames_train) %>%
  juice() %>%
  # select a few rows with different values
  slice(153:157)

# Slide 37 -------------------------------------------------------
lin_terms <- recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
                      Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
                      Central_Air + Longitude + Latitude,
                    data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) 

nonlin_terms <- lin_terms %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

# Slide 38 -------------------------------------------------------

ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

# Slide 39 -------------------------------------------------------

ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

# Slide 40 -------------------------------------------------------

cv_splits <- cv_splits %>% 
  mutate(nonlin_terms = map(splits, prepper, recipe = nonlin_terms))

# Slide 41 -------------------------------------------------------

spec_lm <- linear_reg() %>%
  set_engine("lm")

lm_fit_rec <- function(rec_obj, formula) {
  fit(spec_lm, formula, data = juice(rec_obj))
}

cv_splits <- cv_splits %>% 
  mutate(models = map(
    nonlin_terms, 
    lm_fit_rec, 
    Sale_Price ~ .)
  )

glance(cv_splits$models[[1]]$fit)

# Slide 42 -------------------------------------------------------

assess_predictions <- function(split, recipe, model) {
  raw_assessment <- assessment(split)
  processed <- bake(recipe, new_data = raw_assessment)
  
  model %>%
    predict(new_data = processed) %>%
    bind_cols(processed) %>%
    mutate(
      # Sale_Price is already logged by the recipe
      .resid = Sale_Price - .pred,
      # Save the original row number of the data
      .row = as.integer(split, data = "assessment")
    )
}

# Slide 43 -------------------------------------------------------

cv_splits <- cv_splits %>%
  mutate(
    pred = pmap(
      list(
        split  = splits, 
        recipe = nonlin_terms, 
        model  = models
      ),
      assess_predictions 
    )
  )

# Slide 44 -------------------------------------------------------

# Compute the summary statistics
cv_splits %>%
  unnest(pred) %>%
  group_by(id) %>%
  metrics(truth = Sale_Price, estimate = .pred) %>%
  group_by(.metric) %>%
  summarise(
    resampled_estimate = mean(.estimate)
  )

# Slide 45 -------------------------------------------------------

assess_pred <- cv_splits %>%
  unnest(pred) %>%
  mutate(
    Sale_Price = 10^Sale_Price,
    .pred = 10^.pred
  ) 

ggplot(assess_pred,
       aes(x = Sale_Price,
           y = .pred)) +
  geom_abline(lty = 2) +
  geom_point(alpha = .4)  +
  geom_smooth(se = FALSE, col = "red")

