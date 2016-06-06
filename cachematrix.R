# The first function, makeCacheMatrix creates a special "vector", 
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function assumes that the matrix is always invertible.
# The following function returns the inverse of given matrix. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data.")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data)
  x$setinverse(invrs)
  invrs
}

# Test
# 
# x <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# >
# > m = makeCacheMatrix(x)
# > 
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# >
# > cacheSolve(m)
# getting cached data.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
