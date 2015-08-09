## These functions will create a matrix to be stored for later use, and 
## then invert it and cache the new inverted value in case we need it again.

## This function, makeCacheMatrix, takes one argument(matrix) and creates a special "matrix" 
## object that can cache it's inverse.

makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setCache <- function(inverse) {m <<- inverse}
    getCache <- function() {m}
    list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## This function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Check if there is anything stored in the cache. if there is, return that value
    m <- x$getCache()  
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if the cache is empty, return a matrix that is the inverse of 'x' and store it in the cache
    n <- x$get()
    m <- solve(n, ...)
    x$setCache(m)
    m
}










