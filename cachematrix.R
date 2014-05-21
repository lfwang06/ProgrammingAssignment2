## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly

## The following fuction creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matric()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The next function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache and skip the computation. Otherwise, it computes the inverse 
## of the matrix and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
