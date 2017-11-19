## Caching the Inverse of a Matrix

## This function creates a special "Matrix" that is really a list containing a function
## to set the value of the matrix, get the value of the matrix, set the value of the inverse matrix,
## and get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {

  m<- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  get<- function () x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list (set = set, get = get, setSolve = setSolve, getSolve = getSolve)
  
}


## The following function computes the inverse of the special "matrix" made by the above function
## It check first if it's already been calculated, in which case will return the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<- x$getSolve()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
