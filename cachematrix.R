## Through the functions dedicated to storing the functions that store an inverse matrix in the cache, the MakeCacheMatrix call is used.
makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#result of calculating the inverse of an array by calling makeCacheMatrix, making use of additional arguments to end with the necessary value using CacheSolve (x)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  value <- x$get()
  inv <- solve(value, ...)
  x$setinverse(inv)
  inv
}
