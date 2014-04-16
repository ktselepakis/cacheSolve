## Matrix inversion using a caching inverse 

## This function creates the 'special' matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## Computes the inverse of my special matrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
