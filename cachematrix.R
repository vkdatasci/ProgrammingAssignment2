
## creates a matrix object with it's inverse

makeCacheMatrix <- function(x = matrix()) {
    mtx <- NULL
    set <- function(y) {
        x <<- y
        mtx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mtx <<- inverse
    getinverse <- function() mtx
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # cache lookup
        if (!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        # compute inverse
        m <- solve(x$get(), ...)
        # add inverse to cache
        x$setinverse(m)
        m
}
