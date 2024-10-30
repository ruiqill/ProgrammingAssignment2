## makeCacheMatrix function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse matrix cache  
  # Set the matrix value and clear the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse whenever the matrix is changed
  }
  
  # Get the matrix value
  get <- function() x
  
  # Cache the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of functions for setting/getting the matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to retrieve the cached inverse
  
  # If the cached inverse exists, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse of the matrix
  data <- x$get()
  inv <- solve(data, ...)  # Calculate the inverse
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
