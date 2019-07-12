##FUNCTIONS THAT CAN TAKE THE INVERSE OF A MATRIX



## Program that creates a matrix that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize objects
  inv <- NULL
  
  ## METHODS
  
  ## Set the matrix
  set <- function( matrix ) {
    x <<- matrix
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return a list of the methods
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the Matrix
cacheSolve <- function(x, ...) {
  
   inv <- x$getInverse()
  
 
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
 
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inv <- solve(data, ...)
 
  x$setInverse(inv)
  
  ## Return the matrix
  inv
}
