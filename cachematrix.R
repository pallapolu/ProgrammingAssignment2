## These functions enable caching the inverse of a matrix  avoiding costly computation.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
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

## cacheSolve computes the inverse of the special "matrix" if not cached already

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse of matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(x)
  x$setinverse(inv)
  inv
}
