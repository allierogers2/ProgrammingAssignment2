## A pair of functions that calculates and caches the inverse of a 
## matrix, or returns the inverse from the cache if it already exists

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of the matrix created in makeCacheMatrix.
## If the inverse has already been calculated, get the inverse from the cache.
## Otherwise, calculate the inverse and set the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        m <- x["getinverse()"]
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
