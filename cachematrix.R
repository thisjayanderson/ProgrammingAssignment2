# Anderson
## Caches a matrix inversion and the checks to avoid re-running it when needed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      # Set null matrix cache
      matrixCache <- NULL
      
      # Put value in cache
      matrixSet <- function(y) 
      {
            x <<- y
            
            # Empty cache
            matrixCache <<- NULL
      }
      
      # Store matrix
      matrixGet <- function()
      {
            x      
      }
      
      # Store the function argument
      setInverse <- function(solve) 
      {
            matrixCache <<- solve      
      }     
      
      # Retrieve the inverted matrix
      getInverse <- function() 
      {
            matrixCache
      }
      
      # Return the special vector (it's a list)
      list(matrixSet = matrixSet, 
           matrixGet = matrixGet,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
      
      # Get the value from the cache
      matrixCache <- x$getInverse()
      
      # Check if cache is null, if not, then return cache
      if(!is.null(matrixCache)) 
      {
            message("retrieving cached data")
            return(matrixCache)
      }
      
      # If it is null, then run the inversion calcuation and return the value
      calcReturn <- x$matrixGet()
      matrixCache <- solve(calcReturn, ...)
      x$setInverse(matrixCache)
      
      # Return final value
      matrixCache
}
