## make cache matrix

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invert <<- inverse
    getinverse <- function() invert
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cachesolve function

cacheSolve <- function(x, ...) {
    invert <- x$getinverse()
    if(!is.null(invert)) {
        message("getting cached data:")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data)
    x$setinverse(invert)
    invert
}
