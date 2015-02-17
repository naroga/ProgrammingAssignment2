## This simple program computes matrix inversion and stores
## the result in cache, so we don't have to recompute
## a matrix's inverse when we need it.

## Creates a cached matrix object
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(iMatrix) inverseMatrix <<- iMatrix
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Solves the matrix inverse. If one is already solved for
## the given matrix (x), this function will just return the 
## previously computed inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
