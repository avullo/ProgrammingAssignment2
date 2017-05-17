## Caching the Inverse of a Matrix
##
## An R script with two functions allowing to compute the inverse of a matrix
## and return the (possibly cached) value of it.

## A proof of the correct working of these functions:
##
## > source("cachematrix.R")
## > m <- rbind(c(1, -1/4), c(-1/4, 1)) 
## > cm <- makeCacheMatrix(m)
## > cm$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## Have not yet computed the inverse so it should be NULL
## > cm$getinverse()
## NULL

## get the inverse, no caching so no message
## > cacheSolve(cm)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## get the inverse from the cache
## > cacheSolve(cm)
## getting cached data
##      [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## change the matrix
## > cm$set(rbind(c(4,7),c(2,6)))

## inverse should be NULL
## > cm$getinverse()
## NULL

## compute inverse of the changed matrix, no caching
## > cacheSolve(cm) 
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## get the cached result
## > cacheSolve(cm)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## makeCacheMatrix
## create a special object that stores a numeric matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## 'x' is the matrix which we want to compute the inverse of,
  ## the matrix is assumed to be invertible
  
  ## Returns a list with the following (named) elements:
  ## get: function to get the value of the matrix
  ## set: function to set the value of the matrix
  ## getinverse: function to get the value of the inverse of the matrix
  ## setinverse: function to set the value of the inverse of the matrix
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(get = get, set = set,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
## Returns the inverse of the matrix stored in an object created with the makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), it retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## 'x' is an object created by calling the makeCacheMatrix function
  
  ## Return a matrix that is the inverse of the matrix stored in 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  X <- x$get()
  inv <- solve(X, ...)
  x$setinverse(inv)
  inv
}
