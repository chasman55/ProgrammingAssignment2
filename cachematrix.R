## This function creates a list of functions that make use of the "<<-" operator to cache values of a 
## matrix inversion function in an environment different from the current one and retrieve them so that 
## previous runs with identical input will use cached values instead of recalculating the inverse.

makeCacheMatrix <- function(x = Matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This Function works with output from the makeCacheMatrix function (which is a list of functions) to return the value
## of an inverted matrix; checking first for the existence of a previously executed & cached value of the inversion so that
## needless repetition of the calculation is avoided.

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