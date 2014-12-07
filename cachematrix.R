## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix uses the <<- operator to assign a value to m that is different from the current environment
## function creates cacheable matrix for inputting to cacheSolve() that sets/gets the cached values

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## if inverse has already been calculated then cachesolve retrieves the inverse from the cache
## checks to see if the inverse has already been calculated. If so, it gets the inverse matrix from the cache
## ... else it calculates the inverse of the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
