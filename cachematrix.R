## makeCacheMatrix, cacheSolve
## 23 Jan 2015
## These functions create a way for the user to create a object of a matrix that has the
## ability to provide a cache-able solution of the matrix function solve (Inverse), thereby decreasing 
## the overall computation need if the same object is asked to be inversed again.  

## Creates a object of a matrix that also stores the inverse of itself, once set.
## Args:
##    x: A valid matrix that is always invertable
## Returns:
##    set(y): Sets the x matrix with y. y: is a matrix that is always invertable.
##    get(): Returns the matrix x. Not inverted.
##    setInverse(solve): Sets the inverseX matrix with solve. It is assumed that this matrix is already inversed.
##    getInverse(): Returns the inverseX matrix. 
makeCacheMatrix <- function(x = matrix()) {
  
  inverseX <- NULL # The holder for the inverse of X matrix.
  
  # The set function for the X matrix
  set <- function(y) {
    x <<- y
    invserseX <<- NULL
  }
  # The get function for the X matrix
  get <- function() x
  
  # The set function for the inverse of X matrix (inverseX)
  setInverse <- function(solve) inverseX <<- solve
  
  # The get function for the inverseX matrix
  getInverse <- function() inverseX
  
  # The functions avaliable to this object.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## A cached-able version of the R solve command from object created by the makeCacheMatrix.
## This function returns a Inverse of the matrix provided. The matrix provided needs to be a 
## object of the makeCacheMatrix function. WARNING: This function uses a caching framework to decrease
## computation need if the makeCacheMatrix object is already set in the environment.
## Args:
##    x: A valid object from the makeCacheMatrix function.
## Returns:
##    The inverse of the x matrix provided. 
cacheSolve <- function(x, ...) {
  # Get the inverseX from the makeCacheMatrix object.
  inverseX <- x$getInverse()
  
  # Check the makeCacheMatrix object see if there is already a inverseX set.
  if(!is.null(inverseX)) {
    message("getting cached data")
    # Just return the inverseX if already found, no need to compute again.
    return(inverseX)
  }
  
  # Since we don't have a inverseX already, lets create one and save it.
  # Get the matrix from makeCacheMatrix object.
  theMatrix <- x$get()
  
  # Compute the inverse
  inverseX <- solve(theMatrix, ...)
  
  # Set it in the object so it can be found later.
  x$setInverse(inverseX)
  
  # Return the inverse as promised by the function.
  inverseX
}