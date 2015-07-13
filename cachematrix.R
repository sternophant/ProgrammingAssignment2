## Date:   2015-07-11
## Author: sternophant

## This file contains two functions ("makeCacheMatrix" and "cacheSolve") 
## allowing to calculate and cache the inverse of a square matrix.


## This function creates a special "matrix" object that can cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  ## This method stores the matrix within the environment of the "matrix" object.
  set <- function(y) {
    
    x <<- y
    inverse <<- NULL
    
  }
  
  
  ## This method returns the matrix stored within the environment of the "matrix" object.
  get <- function() x
  
  
  ## This method stores the inverse of the matrix within the environment of the "matrix" object.
  set_inverse <- function(calculated_inverse) inverse <<- calculated_inverse
  
  
  ## This method returns the matrix stored within the environment of the "matrix" object.
  get_inverse <- function() inverse
  
  
  ## Return "matrix" object (list) containing the methods defined above. 
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## This function returns the inverse of a "matrix" object created with the makeCacheMatrix function.
## If the inverse is cached, this function will return the cached inverse.
## If the inverse is not cached, this function will calculate, cache, and return the inverse.
cacheSolve <- function(x, ...) {
  
  # Get the inverse matrix cached within the "matrix" object.
  inverse <- x$get_inverse()
  
  # Check if a inversed matrix is already available in the cache
  if (!is.null(inverse)){
   
    message("Cached inverse:")
    
  } else {
    
    # Get the matrix stored within the "matrix" object.
    matrix <- x$get()
    
    # Calculate the inverse matrix.
    inverse <- solve(matrix, ...)
    
    # Store the inverse matrix within the "matrix" object.
    x$set_inverse(inverse)
    
    message("Calculated inverse")
    
  }
  
  inverse
  
}
