# Slides for Applied Machine Learning workshop 

# Part_5_Classification.R

# Slide 3 --------------------------------------------------------

library(tidymodels)
thm <- theme_bw()
theme_set(thm)
library(caret)

# Slide 5 --------------------------------------------------------

two_class_example %>% head(4)

# Slide 6 --------------------------------------------------------

two_class_example %>% 
	conf_mat(truth = truth, estimate = predicted)

two_class_example %>% 
	accuracy(truth = truth, estimate = predicted)

# Slide 10 --------------------------------------------------------

roc_obj <- 
  two_class_example %>% 
  roc_curve(truth, Class1)

two_class_example %>% roc_auc(truth, Class1)

autoplot(roc_obj) + thm

# Slide 13 --------------------------------------------------------

load("Data/okc.RData")
okc_train %>% dim()
okc_test %>% nrow()
table(okc_train$Class)

# Slide 16/17 -----------------------------------------------------

# Optional - make take a while to run

no_samp <- trainControl(
	method = "cv",
	classProbs = TRUE,
	summaryFunction = twoClassSummary
)
down_samp <- trainControl(
	method = "cv",
	classProbs = TRUE,
	summaryFunction = twoClassSummary,
	sampling = "down"
)

set.seed(5515)
norm_mod <- train(
	x = okc_train[, names(okc_train) != "Class"], 
	y = okc_train$Class,
	method = "treebag",
	metric = "ROC",
	trControl = no_samp
)
set.seed(5515)
down_mod <- train(
	x = okc_train[, names(okc_train) != "Class"], 
	y = okc_train$Class,
	method = "treebag",
	metric = "ROC",
	trControl = down_samp
)

test_norm <- 
	data.frame(Truth = okc_test_big$Class,
						 stem = predict(norm_mod, okc_test_big , type = "prob")[,1],
						 Sampling = "No Sampling",
						 stringsAsFactors = FALSE)
test_down <- 
	data.frame(Truth = okc_test_big$Class,
						 stem = predict(down_mod, okc_test_big , type = "prob")[,1],
						 Sampling = "Down-Sampled",
						 stringsAsFactors = FALSE)

roc_norm <- roc(test_norm$Truth, test_norm$stem, levels = rev(levels(test_norm$Truth)))
roc_down <- roc(test_down$Truth, test_down$stem, levels = rev(levels(test_down$Truth)))

test_prob <- bind_rows(test_norm, test_down) %>%
	mutate(Sampling = factor(Sampling, levels = c("No Sampling", "Down-Sampled")))

ggplot(test_prob, aes(x = stem)) +
	geom_histogram(binwidth = .04, aes(fill = Truth), alpha = 0.6, position = "identity") + 
	facet_wrap( ~ Sampling) + 
	theme(legend.position = "top") + 
	xlab("Prob[Profile is STEM]")

plot(
	roc_norm,
	print.thres = .5,
	print.thres.pattern = "cut = %.2f (Sp = %.3f, Sn = %.3f)\nNo Sampling",
	print.thres.cex = .8,
	legacy.axes = TRUE
)
plot(
	roc_down,
	print.thres = .5,
	col = "blue",
	add = TRUE,
	print.thres.pattern = "cut = %.2f (Sp = %.3f, Sn = %.3f)\nDown-Sampled",
	print.thres.cex = .8,
	print.thres.col = "blue",
	legacy.axes = TRUE
)

# Slide 18 --------------------------------------------------------

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

# Slide 24 --------------------------------------------------------

set.seed(5515)
cart_mod <- train(
	x = okc_train[, names(okc_train) != "Class"], 
	y = okc_train$Class,
	method = "rpart2",
	metric = "ROC",
	tuneGrid = data.frame(maxdepth = 1:20),
	trControl = ctrl
)

# Slide 25 --------------------------------------------------------

cart_mod$finalModel

# Slide 26 --------------------------------------------------------

ggplot(cart_mod)

# Slide 27 --------------------------------------------------------

approx_roc_curve <- function(x, label) {
  x %>%
    pluck("pred") %>%
    roc_curve(obs, stem) %>%
    mutate(model = label)
}
approx_roc_curve(cart_mod, "CART") %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path()  +
  geom_abline(col = "red", alpha = .5)

# Slide 28 --------------------------------------------------------

confusionMatrix(cart_mod)

# Slide 29 --------------------------------------------------------

cart_imp <- varImp(cart_mod, scale = FALSE, 
                   surrogates = FALSE, 
                   competes = FALSE)
ggplot(cart_imp, top = 7) + xlab("")

# Slide 37 --------------------------------------------------------

set.seed(5515)
cart_bag <- train(
	x = okc_train[, names(okc_train) != "Class"], 
	y = okc_train$Class,
	method = "treebag",
	metric = "ROC",
	trControl = ctrl
)

# Slide 38 --------------------------------------------------------

cart_bag

# Slide 39 --------------------------------------------------------

confusionMatrix(cart_bag)

# Slide 40 --------------------------------------------------------

all_curves <-
  approx_roc_curve(cart_mod, "CART") %>%
  bind_rows(approx_roc_curve(cart_bag, "Bagged CART"))

ggplot(all_curves) +
  aes(x = 1 - specificity, y = sensitivity,
      group = model, col = model) +
  geom_path()  +
  geom_abline(col = "red", alpha = .5)

# Slide 41 --------------------------------------------------------

bag_imp <- varImp(cart_bag, scale = FALSE)
ggplot(bag_imp, top = 30) + xlab("")

# Slide 48 --------------------------------------------------------

ggplot(okc_train, aes(x = essay_length, col = Class)) + 
  geom_line(stat = "density", adjust = 1.5)	+
  ylab("density") + 
  xlab("log Essay Length") 

# Slide 50 --------------------------------------------------------

okc_train %>% 
  group_by(religion, Class) %>% 
  count() %>% 
  dplyr::rename(per_religion = n) %>% 
  inner_join(okc_train %>% group_by(Class) %>% count()) %>% 
  mutate(Prob = per_religion/n) %>% 
  ggplot(aes(x = reorder(religion, Prob), y = Prob, fill = Class)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  xlab("") + 
  ylab("Within-Class Probability") + 
  theme(legend.position = "top")
  

# Slide 55 --------------------------------------------------------

is_dummy <- function(x) length(unique(x)) == 2 & is.numeric(x)
dummy_or_not <- map_lgl(okc_train, is_dummy)
dummies <- names(dummy_or_not)[dummy_or_not]
head(dummies)
no_dummies <- 
  recipe(Class ~ ., data = okc_train) %>%
	step_bin2factor(!!! dummies) %>%
	step_zv(all_predictors())

nb_grid <- expand.grid(usekernel = TRUE, fL = 0, adjust = 1)

# Slide 56 --------------------------------------------------------

set.seed(5515)
nb_mod <- train(
	no_dummies,
	data = okc_train,
	method = "nb",
	metric = "ROC",
	tuneGrid = nb_grid,
	trControl = ctrl
)

# Slide 57 --------------------------------------------------------

nb_mod

# Slide 58 --------------------------------------------------------

all_curves <-
  all_curves %>%
  bind_rows(approx_roc_curve(nb_mod, "Naive Bayes"))

ggplot(all_curves) +
  aes(x = 1 - specificity, y = sensitivity,
      group = model, col = model) +
  geom_path()  +
  geom_abline(col = "red", alpha = .5)

# Slide 59 --------------------------------------------------------

test_res <- okc_test %>%
	dplyr::select(Class) %>%
	mutate(
		prob = predict(nb_mod, okc_test, type = "prob")[, "stem"],
		pred = predict(nb_mod, okc_test)
	)

roc_auc(test_res, Class, prob)

getTrainPerf(nb_mod)

# Slide 60 --------------------------------------------------------

two_class <- metric_set(sens, spec, accuracy)
two_class(test_res, truth = Class, estimate = pred)

test_roc <- roc_curve(test_res, Class, prob)

ggplot(test_roc) +
  aes(x = 1 - specificity, y = sensitivity) +
  geom_path()  +
  geom_abline(col = "red", alpha = .5)

# Slide 61 --------------------------------------------------------

ggplot(test_res, aes(x = prob)) + 
  geom_histogram(binwidth = .04) + 
  facet_wrap( ~ Class)


