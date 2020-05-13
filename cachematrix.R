## This R script consists of two functions that, combined can evaluate
## the inverse of a square matrix, the matrix for which the inverse
## is being evaluated is always assumed to be a square matrix


## A function that takes a matrix object as input, and creates a list of 
## four functions, get(), set(), get_inverse() and set_inverse() for getting
## and setting the matrix itself and getting and setting the inverse of 
## the matrix respectively

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) minv <<- inverse 
  get_inverse <- function() minv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## a function for calculating the inverse of a matrix, and caching the value
## so that when it's called again it doesn't re-evaluate the inverse and instead
## simply retrieve the data from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$get_inverse()
  if (!is.null(minv)){
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data,...)
  x$set_inverse(minv)
  minv
  
}
