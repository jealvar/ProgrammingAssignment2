## This function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix (solve)
## get the value of the inverse matrix (solve)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      ## set the value of the matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## get the value of the matrix
      get <- function() x
      
      ## set the value of the inverse matrix (solve)
      setsolve <- function(solve) m <<- solve
      
      ## get the value of the inverse matrix (solve)
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
