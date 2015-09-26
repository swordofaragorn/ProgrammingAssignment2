## This set of functions caches the inverse of a matrix to avoid having to repeatedly calculate it.

## makeCacheMatrix creates a list of functions to set the value of a matrix, get the value of a matrix,
## set the inverse of the matrix, and get the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix, if the inverse
## is not already cached.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
