## makeCacheMatrix function create getter (access) and setter (mutate) functions
## to access and mutate two objects matrix and inverse of matrix
## makeCacheMatrix cache the inverse Matrix

makeCacheMatrix <- function(x = matrix(numeric())) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse of matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}