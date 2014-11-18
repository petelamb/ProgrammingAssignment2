# The functions below will cache the inverse of a matrix, and then retrieve the
# already cached matrix inversion to save repeated computation.

# Create the floating variable, m, initialize it with NULL. Create and assign
# the functions set(), get(), setMat(), and getMat() to a list and output when
# makeCacheMatrix() is called.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setMat <- function(solve) m <<- solve
      getMat <- function() m
      list(set = set, get = get,
           setMat = setMat,
           getMat = getMat)
}


# The list of functions from makeCacheMatrix is used as input for cacheSolve.
# The cached matrix is retrieved, inverted and output. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getMat()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setMat(m)
      m
}
