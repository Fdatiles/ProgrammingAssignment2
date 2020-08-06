## makeCacheMatrix(x) : creates a vector containing the matrix x and its
## inverse, accessible through $getinv()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        return(x)
    }
    setinv <- function(minv) {
        inv <<- minv
    }
    getinv <- function() {
        return(inv)
    }
    return(list(set = set,
                get = get,
                setinv = setinv,
                getinv = getinv))
}


## cacheSolve(x, ...) : finds the inverse of x if it's not cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    if (!is.null(minv)) {
        message("getting cached data")
        return(minv)
    } else {
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        return(minv)
    }
}
