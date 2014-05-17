##  Functions for creating and manipulating an object, which contains a matrix and it's cached mean.
##  The objects inverse is calculated and stored on the first call to the cacheSolve() method. All subsequent
##  calls to cacheSolve() return the cached value unless the set() method on the object is called, which invalidates
##  the cached inverse value and it is calculated from scratch for the new matrix on the next call to cacheSolve()
##

## Creates the 'catche matrix' object.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseX) cachedInverse <<- inverseX
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## gets the inverse of the matrix x. Calculates it on the first run and retrieves a catched value
## on all subsequent calls until the set method is called on the matrix object.

cacheSolve <- function(x, ...) {
  inverseX <- x$getInverse()
  if(!is.null(inverseX)) {
    message("getting cached inverse")
    return(inverseX)
  }
  data <- x$get()
  inverseX <- solve(data, ...)
  x$setInverse(inverseX)
  inverseX
}