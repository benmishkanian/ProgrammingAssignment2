## makeCacheMatrix can be used to create an object that caches the
## inverse of a matrix. The inverse can then be computed using cacheSolve.

## Creates a special matrix object that caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the CacheMatrix object x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    if (!is.null(inverseMatrix)) {
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
