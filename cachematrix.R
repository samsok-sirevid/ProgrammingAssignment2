## Finds and caches the inverse of a matrix for future reference.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x.inv <- NULL
  set <- function(y){
    x <<- y
    x.inv <<-NULL
  }
  get <- function() x
  setinv <-function(solve) x.inv <<- solve
  getinv <- function() x.inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  x.inv <- x$getinv()
  if(!is.null(x.inv)){
    message("getting cached data")
    return(x.inv)
  }
  data <- x$get()
  x.inv <-solve(data, ...)
  x$setinv(x.inv)
  x.inv
}

A = matrix(1:4,2)
x<-makeCacheMatrix(A)
cacheSolve(x)
cacheSolve(x)
