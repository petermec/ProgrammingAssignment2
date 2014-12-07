## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix uses the <<- operator to assign a value to m that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## if inverse has already been calculated then cachesolve retrieves the inverse from the cache

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
