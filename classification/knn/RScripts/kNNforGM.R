## kNN for Geometric Morphometrics

## kNN is one of the easiest Machine Learning algorithms (ML) to implement for the purposes of work in geometric morphometrics (GM), 
## in part because kNN is easy to implement, and in part because many of the conceptual questions are already answered for you: 
## Generalized Procrustes Analysis, the most common method of superimposition in GM involves projecting the coordinates into a euclidean 
## tangent space to allow the use of the l2 norm to calculate Procrustes Distances.  With this conceptual question out of the way, we may 
## focus on how we wish to implement the procedure, not on identifying an appropriate metric for the space.  In this script, we show an 
## easy implementation of kNN of even greater generality than this (it is able to implement any p-norm, including l2) but which is able 
## to do the work we are interested in, and we apply it to a real-life dataset (Houle et al 2017, available on Dryad Digital Repository, 
## an academic data sharing service for biological data).

## For those of you who do not know what geometric morphometrics are but are here because you want to try your hand at Data Science/ML, 
## the basic idea is this: you're taking a bunch of biological samples and trying to describe their shape by looking at 'landmarks' that 
## are shared between the wings, taking their X-Y(-Z) coordinates, adjusting them all to be the same size (size != shape afterall), and 
## trying to make them align as closely as possible without warping their shape.  Then the results are projected into a euclidean space 
## so that we can use a simple and computationally easy distance measure (Procrustes Distances, basically Euclidean distances  for shape 
## data).  This allows us to quantify differences in shape, esp. between biological organisms. 

set.seed(12) ## Make stochastic calculations reproducible

library(data.table) ## Read in the Houle data quickly
library(geomorph) ## Perform Generalized Procrustes Analysis and related data cleaning quickly
source("../materialsKNNForGM/kNNFunctions.R")

Houle <- fread("../materialsKNNForGM/Houle_et_al_2017.csv")
Houle <- Houle[grepl("MELANOGASTER", Houle$Species), ] ## Since Houle considers >100 species we will narrow down our analysis somewhat 
## (though a good follow up would be the multi-cluster problem of classifying individuals into each species)

Coords <- arrayspecs(Houle[ , 4:27], p = 12, k = 2)

GPAHoule <- two.d.array(gpagen(Coords, print.progress = FALSE)$coords) ## Perform Generalized Procrustes Analysis on the dataset and 
## convert to useful format.

Sex <- Houle$Sex ## The sex of the fly for each row of the coordinate matrix

Fem <- sum(Sex == "F") ## Number of females
Male <- sum(Sex == "M") ## Number of males

FemaleSample <- sample(which(Sex == "F"), size = as.integer(Fem/8)) ## Let's set aside an eight of both sexes for the test group
MaleSample <- sample(which(Sex == "M"), size = as.integer(Male/8))

TrainingData <- GPAHoule[-c(FemaleSample, MaleSample), ] ## The data we will use for classification
TrainingSex <- Sex[-c(FemaleSample, MaleSample)]

TestData <- GPAHoule[c(FemaleSample, MaleSample), ] ## The data we will use to test whether the classification worked well
TestActualClassification <- Sex[c(FemaleSample, MaleSample)]

TestKNNClassification5 <- rep(NA_character_, length(TestActualClassification))

for (i in seq_along(TestKNNClassification5)) { 
  ## Classify each of our test points based on the training data
  ## k = 5 means we do not have to consider the case of a tie, we are letting each of the five nearest neighbours 'vote'
  ## p = 2 because that is the euclidean norm
  NN5 <- kNN(TrainingData, TrainingSex, TestData[i, ], k = 5, p = 2)$result
  TestKNNClassification5[i] <- names(NN5[which.max(NN5)])
}

rm(NN5)

ProportionPredictedCorrectlyNN5 <- mean(TestActualClassification == TestKNNClassification5)
NullExpectedCorrectPredictions <- mean(Sex == "F")^2+mean(Sex == "M")^2 ## Based on the assumption that they individuals will be 
## identified in the same proportions they are in the original sample, but at random.
## So the result is that we correctly predicted the sex of an individual in this sample 4/5ths of the time (possibly that low because 
## there are more females in the sample since it is unbalanced and so we correctly predicted almost all the females correctly and 
## predicted some of the males were female).  Let's try a few other levels of prediction.  In order to correct for this, I am going to try 
## a smaller number, just using nearest neighbours.

TestKNNClassification1 <- rep(NA_character_, length(TestActualClassification))

for (i in seq_along(TestKNNClassification1)) {
  ## We only let the very nearest neighbour vote here
  NN1 <- kNN(TrainingData, TrainingSex, TestData[i, ], k = 1, p = 2)$result
  TestKNNClassification1[i] <- names(NN1[which.max(NN1)])
}

rm(NN1)

ProportionPredictedCorrectlyNN1 <- mean(TestActualClassification == TestKNNClassification1)
## This is slightly higher (exactly 5/6ths), but as to be expected, with an increased true positive rate for males, there is a higher 
## false negative rate for females.

## Unfortunately we cannot produce classification maps like I made in our previous practice script since the data is 24 dimensional (see 
## ncol(GPAHoule)).  It would be possible to do this analysis over with the first two Principal Components (at least one of which is 
## almost assuredly related to sex due to the sexual dimorphism in Fruit Fly wings) and then classify in that space and produce 
## classification maps, but that is outside the scope of our work here.
