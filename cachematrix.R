## Put comments here that give an overall description of what your
## functions do

## Here, we create the special matrix just as the example in the special vector

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL ##As it is a new value that it is set, we set the mean back to null
  }
  get <- function() x
  setinv <- function(newInv) inv <<- newInv ##We assign the new value
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Here, we create the cache so that we don't have to calculate again the inverse if it was already calculated

cachesolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ##We call the solve function as recommended in the assignment for calculating the inverse
  x$setinv(inv)
  inv
}
