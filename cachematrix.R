## Takes a matrix and assigns different functions to it with the ability to store
## the matrix itself and its inverse in the cache

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


## First checks if the inverse to the matrix provided to makeCacheMatrix() exists,
## then prints if it does, and solves for it and stores it if it doesn't 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}