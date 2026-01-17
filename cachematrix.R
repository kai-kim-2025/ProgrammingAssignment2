## pair of functions that cache the inverse of a matrix to
## avoid potential repetition and costly computations

## creates a special "vector" that has a list of functions
## to set and get the value of the matrix as well as
## to set and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inverseOfX <- NULL
      
      setMatrix <- function(mtrx) {
            x <<- mtrx
            inverseOfX <<- NULL
      }
      
      getMatrix <- function() x
      
      setInverse <- function(inv_mtrx) inverseOfX <<- inv_mtrx
      
      getInverse <- function() inverseOfX
      
      list(setMatrix = setMatrix, getMatrix = getMatrix, 
           setInverse = setInverse, getInverse = getInverse)
}


## returns the inverse matrix of the special "vector" created
## from makeCacheMatrix. skips the computation if the inverse
## matrix has already been computed

cacheSolve <- function(x, ...) {
      
      inv_x <- x$getInverse()
      
      if (!is.null(inv_x)) {
            message("getting cached inverse")
            return(inv_x)
      }
      
      mtrx <- x$getMatrix()
      inv_x <- solve(mtrx)
      x$setInverse(inv_x)
      
        ## Return a matrix that is the inverse of 'x'
      inv_x
}
