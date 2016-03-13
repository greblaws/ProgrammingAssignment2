## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## input - x is a  a square invertible matrix
## output a list containing functions to
##  -  set the matrix
##  -  get the matrix
##  -  set the inverse
##  -  get the inverse
##  this list is the input to cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    # <<- makes the variable more 'global' 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse 
  getinv <- function() m
  list(set=set
     , get=get
     , setinv=setinv
     , getinv=getinv)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
# get it from the cache to skip the computation if computed
cacheSolve <- function(x, ...) {
  m = x$getinv()
  # if the inverse has already been calculated
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # otherwise, calculate the inv 
  data <- x$get()
  inv <- solve(data, ...)
  # set inverse in the cache via the setinv
  x$setinv()
  m
}
