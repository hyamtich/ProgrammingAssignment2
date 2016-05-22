## these functions will allow you to avoid finding the inverse
## of a function if the inverse has already been calculated

## this function creates a new "special matrix" that includes
## a matrix, its inverse (if this has been calculated)
## and functions to return information about the matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y){
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) x_inverse <<- inverse 
  get_inverse <- function() x_inverse
  list(set = set, get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## this function calculates the inverse of the matrix
## after checking to see whether the inverse has been calculated already

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse))
  {
    print("Getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}
