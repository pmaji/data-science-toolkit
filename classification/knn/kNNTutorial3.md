## k-Nearest Neighbours Tutorial 3

The k-nearest neighbours function I developed in a previous script was developed in order to illustrate the k-nearest neighbours algorithm. In this script on the other hand, we will use the class library. The class library offers multiple functions for the purposes of classification, using k-NN but with many functions for the purposes of doing cross-validation, etc.

Consider the R code for the function:

    ## function (train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE) 
    ## {
    ##     train <- as.matrix(train)
    ##     if (is.null(dim(test))) 
    ##         dim(test) <- c(1, length(test))
    ##     test <- as.matrix(test)
    ##     if (any(is.na(train)) || any(is.na(test)) || any(is.na(cl))) 
    ##         stop("no missing values are allowed")
    ##     p <- ncol(train)
    ##     ntr <- nrow(train)
    ##     if (length(cl) != ntr) 
    ##         stop("'train' and 'class' have different lengths")
    ##     if (ntr < k) {
    ##         warning(gettextf("k = %d exceeds number %d of patterns", 
    ##             k, ntr), domain = NA)
    ##         k <- ntr
    ##     }
    ##     if (k < 1) 
    ##         stop(gettextf("k = %d must be at least 1", k), domain = NA)
    ##     nte <- nrow(test)
    ##     if (ncol(test) != p) 
    ##         stop("dims of 'test' and 'train' differ")
    ##     clf <- as.factor(cl)
    ##     nc <- max(unclass(clf))
    ##     Z <- .C(VR_knn, as.integer(k), as.integer(l), as.integer(ntr), 
    ##         as.integer(nte), as.integer(p), as.double(train), as.integer(unclass(clf)), 
    ##         as.double(test), res = integer(nte), pr = double(nte), 
    ##         integer(nc + 1), as.integer(nc), as.integer(FALSE), as.integer(use.all))
    ##     res <- factor(Z$res, levels = seq_along(levels(clf)), labels = levels(clf))
    ##     if (prob) 
    ##         attr(res, "prob") <- Z$pr
    ##     res
    ## }
    ## <bytecode: 0x7fee9e1cb5e8>
    ## <environment: namespace:class>

We see near the end the function .C() used to call compiled C++ code, in this case the function VR\_knn built into the package. This is the real heavy lifter, doing the majority of the computationally intensive work. This makes the function kind of blackbox-y like I've said, but compiled C++ code is dramatically faster than code in an interpreted language like R.

We will compare the relative speeds of the function I've shown in previous tutorials with the class library's version to show the degree of improvement. I am going to use the exact same dataset as in tutorial 1.

For this, I will use the benchmark function from the rbenchmark package using k = 5.

``` r
benchmark(kNN(classifiedData = combinedData, 
              classification = groupMembership, 
              unclassifiedPoint = c(1, 1), 
              p = 2, 
              k = 5), 
          knn(train = combinedData, 
              test = c(1, 1), 
              k = 5, 
              cl = groupMembership)
          )
```

    ##                                                                                                              test
    ## 1 kNN(classifiedData = combinedData, classification = groupMembership, unclassifiedPoint = c(1, 1), p = 2, k = 5)
    ## 2                                          knn(train = combinedData, test = c(1, 1), k = 5, cl = groupMembership)
    ##   replications elapsed relative user.self sys.self user.child sys.child
    ## 1          100   0.105    5.833     0.093    0.010          0         0
    ## 2          100   0.018    1.000     0.018    0.001          0         0

``` r
benchmark(kNN(classifiedData = combinedData, 
              classification = groupMembership, 
              unclassifiedPoint = c(1, 1), 
              p = 2, 
              k = 11), 
          knn(train = combinedData, 
              test = c(1, 1), 
              k = 11, 
              cl = groupMembership)
          )
```

    ##                                                                                                               test
    ## 1 kNN(classifiedData = combinedData, classification = groupMembership, unclassifiedPoint = c(1, 1), p = 2, k = 11)
    ## 2                                          knn(train = combinedData, test = c(1, 1), k = 11, cl = groupMembership)
    ##   replications elapsed relative user.self sys.self user.child sys.child
    ## 1          100   0.075    4.412     0.073    0.002          0         0
    ## 2          100   0.017    1.000     0.017    0.000          0         0

What we see is that the class package's function is three to six times faster for a relatively small dataset, a single training point, and a relatively small k on my computer. If we had larger values for the other parameters we would expect larger jumps in those speed enhancements as well. There are other advantages as well, since knn is tested well enough to have a great deal more overhead in place to catch potential errors (like test and training data of different dimensionality - mine would give an indexing error in response to that), more eyes have gone over the code for it, it has more options in certain capacities, and generally it is good practice to use established functions. Mine also can only applied to one point at a time (though it would be very simple to apply it down test dataset).

The knn function in the class library makes one choice that makes life simpler but not necessarily in a good way: it chooses what distance measure you're using. The knn function only does euclidean distances, unlike my implementation which is designed to do any p-norm. However for many purposes this isn't a problem, since for instance in the geometric morphometrics dataset I worked on in the previous tutorial, it was natural to use l2/euclidean distances.

Finally, let's consider the object produced by knn.

``` r
knn(train = combinedData, 
              test = c(1, 1), 
              k = 11, 
              cl = groupMembership)
```

    ## [1] red
    ## Levels: blue red

It produces a factor vector which displays all possibilities as factors, and the actual possibility for each test point in the vector.
