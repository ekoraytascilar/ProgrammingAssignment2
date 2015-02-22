## The makeCacheMatrix function creates a square matrix from a single integer by sampling random normal numbers
## and creates functions that are able to get the assigned variables from the very 
## environment in which they were created.

## A small difference from the original; my makeCacheMatrix function creates a random matrix from an integer 

## x with a dimention of (x,x) that includes x^2 elements drawn from a random distribution with a mean of x and 
## sd of approximately x/2. if you don't pass a non-zero positive integer as an argument it will throw an error.

makeCacheMatrix <- function(x = numeric()) {
  if (as.integer(x)!=x|as.integer(x)<=0){ 
    print ("Not a nice number, need a non-zero positive integer")
    stop
  }
  else n<-matrix(as.integer(rnorm(x^2, mean=x, sd=as.integer(x/2))), c(x,x))
  
  m <- NULL
  set <- function(y) {
    n <<- y
    m <<- NULL
  }
  get <- function() n
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Operation of this neccesitates that output of the makeCacheMatrix function is passed on a vector.
## when you pass this result vector as the argument of cacheSolve, it in the first execution caches the inverse of the 
## matrix created in the previous function and in subsequent re-executions verifies that there's a cached
## inverse matrix value and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached solve")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
