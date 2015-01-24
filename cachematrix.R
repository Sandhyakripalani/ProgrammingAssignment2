## Two functions makeCacheMatrix and cacheSolve are used to cache and retrieve
## the inverse of a matrix

## The makeCacheMatrix funtion initializes a matrix 
## and has methods to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
          matrixInv <- NULL
          set <- function(y) {
            x <<- y
            matrixInv <<- NULL
          }
          get <- function() x
          setmatrixInv <- function(inverse) matrixInv <<- inverse
          getmatrixInv <- function() matrixInv
          list(set = set, get = get,
               setmatrixInv = setmatrixInv,
               getmatrixInv = getmatrixInv)
}


## Return a matrix that is the inverse of 'x'
          
cacheSolve <- function(x, ...) {
         matrixInv <- x$getmatrixInv()
          if(!is.null(matrixInv)) {
            message("getting cached data")
            return(matrixInv)
          }
          matrix <- x$get()
          matrixInv <- solve(matrix)
          x$setmatrixInv(matrix)
          matrixInv
}
