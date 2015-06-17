# These functions deliver the ability to find the inverse of a
# matrix and cache that result. This is done to prevent a performance
# hit each time a matrix is inverted (assuming the data hasn't changed)

# makeCacheMatrix returns a special "matrix" object that implements the
# following functions:
#  set - sets the matrix data and clears the cached inverse
#  get - returns the matrix data
#  setinverse - sets the inverse to the passed parameter
#  getinverse - returns the cached inverse

# To test:
# > m <- makeCacheMatrix(matrix(c(1,3,3,1,4,3,1,3,4), nrow=3, ncol=3, byrow=TRUE))
# > cacheSolve(m)
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
# > cacheSolve(m)
# using cached inverse
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setinverse <- function(i) { inverse <<- i }
  getinverse <- function() { inverse }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve returns the inverse of the passed matrix. It uses the special object above
# to store the inverse once computed. Supsequent resuests for the inverse will return
# the cached inverse, until the underlying matrix data is changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("using cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
