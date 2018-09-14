## KNN functions so that we don't cloud up our practice file with function definitions

distNeighbours <- function(classifiedData, unclassifiedPoint, p = c(1L, 2L, Inf)) { ## This function takes in all the data we have so far,
  ## a point we are interested in looking at, and a p-norm (input can be any natural number or Inf), and it outputs the distance using that
  ## norm from the unclassified point of interest to every classified point.  Since we calculate these values in our KNN implementation, 
  ## I believe this has less-than-ideal computational complexity, so I have labelled this a naive implementation.  It is still "fast 
  ## enough" for the datasets we consider (the ~6400 iterations of it in each nested loop using 200 data points takes about 6 seconds).
  di <- classifiedData
  for (i in 1:length(unclassifiedPoint)) { ## This produces the absolute differences in all dimensions of our point from every other point.
    di[ , i] <- abs(classifiedData[ , i]-unclassifiedPoint[i])
  }
  if (is.finite(p)) { ## If p is a natural number (not the infinity norm)...
    exponentiated <- di^p
    distance <- rowSums(exponentiated)
    distance <- distance^(1/p) ## Then do sum(row^p)^(1/p) (the p-norm), if p=2 this is often called the l2 or Euclidean norm, if p=1 this
    ## is the l1 or taxicab norm.
  }
  else if (is.infinite(p)) { ## If p is Inf (infinity)...
    distance <- apply(di, 1, max) ## Then do max(row), which is the p-norm as p -> +infinity, thus we've implemented every p-norm in this
    ## function, we could have implemented other distance measures as well, but I think these are the most common numeric vector norms, and
    ## also the most relevant for my purposes.
  }
  else { ## And for the indecisive...
    exponentiated <- di^2
    distance <- sqrt(rowSums(exponentiated)) ## We have the l2 or Euclidean norm as a default, but...
    warning("No or unknown distance measure given, defaulting to p=2/euclidean norm.") ## we better warn them of their indecision.
  }
  distance
}

kNN <- function(classifiedData, classification, unclassifiedPoint, k, p = 2) { ## And our piece de resistance, the KNN algorithm
  distances <- distNeighbours(classifiedData, unclassifiedPoint, p) ## Once we have the distances to every point
  kthneighbour <- sort(distances)[k] ## Find the distance to the kth closest point
  kNN <- classification[distances <= kthneighbour] ## And find out which group all the points at most that distance away are
  conclusion <- table(kNN)/k ## Collect the votes (each of those neighbours gets a vote as to what a given point is based on what they 
  ## are) and make the values into proportions
  list(results = conclusion, k = k, p = p) ## And return results, including some information on how we collected the results
}
