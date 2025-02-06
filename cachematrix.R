## These two functions cache the inverse of a matrix

## makeCacheMatriz creates a special object that can cache the inverse of a matrix

makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y 
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function (inverse) inv <<- inverse
  getInverse <- function() inv
  
  llist (set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
  
}


## cacheSolve calculates the inverse of the special matrix created with makeCacheMatrix

cacheSolve <- function (x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message ("getting inverse")
    return (inv)
  }
  
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
