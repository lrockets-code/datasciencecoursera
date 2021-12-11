## LAST RELEASE

## this function sets the matrix and sets the invert at NULL
## also has the method to get the matrix
## also are defined the methods to get and set the inverted matrix 
## with setSolve and getSolve

makeCacheMatrix <- function(matrix = numeric()) {
  invert <- NULL
  set <- function(y) {
    matrix <<- y
    invert <<- NULL
  }
  get <- function() matrix
  setSolve <- function(invert) invert <<- invert
  getSolve <- function() invert
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function works on the cached matrix
## tries to get the inverted from cache and if it is present
## is given
## if is not present is calculated and set and printed finally


cacheSolve <- function(x, ...) {
  solve <- x$getSolve()
  if(!is.null(solve)) {
    message("getting cached data")
    return(solve)
  }
  data <- x$get()
  solve <- solve(data, ...)
  x$setSolve(solve)
  solve
}

