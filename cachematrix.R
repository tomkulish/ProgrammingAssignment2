## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    invserseX <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverseX <<- solve
  getInverse <- function() inverseX
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseX <- x$getInverse()
    if(!is.null(inverseX)) {
      message("getting cached data")
      return(inverseX)
    }
    theMatrix <- x$get()
    inverseX <- solve(theMatrix, ...)
    x$setInverse(inverseX)
    inverseX
}
