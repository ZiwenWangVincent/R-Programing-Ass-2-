## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## Vincent Wang 
## This is my programming assignment 2
makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a matrix object that can cache its inverse.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## Write a short comment describing this function
## This function cauculate the inverse of the matrix made by makeCacheMatrix above.
## If the inverse has already been calculated then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


