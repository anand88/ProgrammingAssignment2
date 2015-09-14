## These functions combine to solve, save a matrix and its inverse

## makeCacheMatrix contains the get set functions for the matrices, and its inverses

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(invMat) m <<- invMat
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve returns the inverse of the matrix from a cache if it has already
## been solved else finds the inverse, saves it in cache and returns the value

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}