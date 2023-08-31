## Return a list of functions for caching the inverse value
## set, to set matrix value and remove cached inverse
## get, get current matrix value
## setinverse, set the inverse of current matrix
## getinverse, get the inverse of current matrix

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


## Return an inverse of a matrix 'x'.
## If the matrix has cached inverse, then use that value instead.
## If the matrix doesn't have cached inverse, then calculate using solve function.

cacheSolve <- function(x, ...) {
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
