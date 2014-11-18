## 
## Contents:
##     function: makeCacheMatrix() - stores matrix inverse in cache
##     function: cacheSolve() - solves inverse of square matrix and sets
##                              in makeCacheMatrix()
##----------------------------------------------------------------------


## This function creates a matrix object that can cache the inverse
## of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
                                                  
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##----------------------------------------------------------------------

## This function solves and sets the inverse matrix to the makeCacheMatrix
## function or, if already computed, retrieves from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
##----------------------------------------------------------------------

## Example usage with output:
#
# > cacheMatrix <- makeCacheMatrix (matrix (c(2,9,3,3,9,2,1,5,7), nrow=3, ncol=3))
# > cacheSolve(cacheMatrix)
# [,1]       [,2]       [,3]
# [1,] -1.1276596  0.4042553 -0.1276596
# [2,]  1.0212766 -0.2340426  0.0212766
# [3,]  0.1914894 -0.1063830  0.1914894
# > cacheSolve(cacheMatrix)
# getting cached data
# [,1]       [,2]       [,3]
# [1,] -1.1276596  0.4042553 -0.1276596
# [2,]  1.0212766 -0.2340426  0.0212766
# [3,]  0.1914894 -0.1063830  0.1914894

# Check code (returns original matrix):
# > invMatrix <- cacheSolve(cacheMatrix)
# getting cached data
# > solve(invMatrix)
# [,1] [,2] [,3]
# [1,]    2    3    1
# [2,]    9    9    5
# [3,]    3    2    7