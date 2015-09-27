## to use the scoping rules of R(lexical scoping) and to cache the inverse of a
## matrix, instead of calculating repeatedly.

## The function makeCacheMatrix creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {              
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv #set the value of inverse
  getinv <- function() m            #get the value of inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function computes inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed),then the cachesolve should retrieve
## the inverse from the cache.


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {         #checks for cached inverse
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)   #computes inverse
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
