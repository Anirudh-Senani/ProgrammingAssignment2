## makeCache data takes an input matrix and returns a list of functions which are
## used in cacheSolve to cache the inverse matrix

## This function returns the list containing functions setmat,getmat,setinv,getinv

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmat <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmat <- function() x
  setinv <- function(invmat) i <<- invmat
  getinv <- function() m
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## This function caches the inverse of matrix in above func and returns the cahched
## inverse matrix if called again without calculating inverse again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getmat()
  i <- solve(data)
  x$setinv(i)
  i
}
