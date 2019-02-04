## Creating two functions:
## 1. Creates a cache matrix with get, set methods.
## 2. Calls the Cache matrix and returns the matrix inverse, if the
## inverse matrix is not available in the Cache matrix then it
## calculates the inverse

## Creates a Cache matrix and returns a list of get/set funtions to work
## with the cacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv){
    invMatrix<<- inv
  }
  getInverse <- function() invMatrix
  list(set = set, get= get
       , setInverse = setInverse
       , getInverse = getInverse)
}


## Utilizes the makeCacheMatrix function to get the inverse matrix
## from the CacheMatrix if already calculated, else calculates the inverse
## matrix and stores the value in the CacheMatrix and returns
## the inverse matrix value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if(!is.null(i)){
    message("Getting inverse data from cache")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
