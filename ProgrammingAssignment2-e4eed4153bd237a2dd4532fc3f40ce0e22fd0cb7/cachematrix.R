## Week3 Assignment

## makeCacheMatrix creates a matrix object and has its inverse cached

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                              
  set <- function(y) {                     
    x <<- y                            
    i <<- NULL                        
  }
  get <- function() x                   
  setinv <- function(inverse) i <<- inverse 
  getinv <- function() i                     
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##cacheSolve checks for cached inverse and retrieves it, or else, calculates 
##a new inverse and prints it

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
