## Finds and caches the inverse of a matrix for future reference.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  x.inv <- NULL   # Introduces and sets x.inv to be NULL
  set <- function(y){
    x <<- y  # Changes x to the global matrix
    x.inv <<-NULL  # Resets x.inv to NULL as matrix changed
  }
  get <- function() x
  
  
  setinv <-function(solve) x.inv <<- solve  #Get the inverse
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
  ## Return a matrix that is the inverse of 'x'
  x.inv <- x$getinv()
  
  # If inverse is already in cache, say so then return the inverse.
  if(!is.null(x.inv)){
    message("getting cached data")
    return(x.inv)
  }
  # Otherwise, find the inverse and store in cache.
  data <- x$get()
  x.inv <-solve(data, ...)
  x$setinv(x.inv)
  x.inv
}

## Just to check:
A = matrix(1:4,2)
x<-makeCacheMatrix(A)
cacheSolve(x)
cacheSolve(x)
