
## cache matrix "class": list with getter / setter
## usage: m <- makeCacheMatrix(c(4,5,6,2), nrow=2)
## 

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


## returns inverse matrix for given matrix "x" from cache
## property "x": as returned by "makeCacheMatrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## cache lookup
        if (!is.null(m)) {
            ## if inverse already availabe, returns from cache
            message("getting cached data")
            return(m)
        }
        ## ... inverse not in cache
        ## compute inverse
        m <- solve(x$get(), ...)
        ## add inverse to cache
        x$setinverse(m)
        m
}
