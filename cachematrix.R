## Two functions that perform memoization of 
##   matrix inversion.

## Creates a vector consisting of a:
##   1. set function (containing a matrix and its inverse)
##   2. get function
##   3. store_inverse function
##   4. get_inverse function
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  store_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  list(set = set, get = get,
       store_inverse = store_inverse,
       get_inverse = get_inverse)
}


## Return a matrix that is the inverse of 'x'
##   (either a cached copy, or a recently computed copy)
cacheSolve <- function(x, ...) {
  # check if the inverse is already stored...
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  # ... if not, compute the inverse and store it for the future
  data <- x$get()
  i <- solve(data, ...)
  x$store_inverse(i)
  i
}
