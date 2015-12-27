## Put comments here that give an overall description of what your
## functions do :
## The pair of functions written below, help cache the inverse of an invertible matrix,
## so that it can be looked up in the cache for future requirement rather than recomputing.

## Write a short comment describing this function
## This function creates a special matrix object, which generates a list containing functions
## to set and get the value of the matrix and to set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(invmatrix) inv <<- invmatrix
  get_inv <- function() inv
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix created with the above function.
## If the inverse has already been calculated, then it retrieves the inverse from the cache.
## Else it gets the data using get function and computes inverse. The computed value is then
## returned to cache using set_inv function. The final output is the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
