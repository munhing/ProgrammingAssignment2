## The purpose of these 2 functions is to cache the Inverse of a square Matrix.
## What this function does not do is verifying the input matrix is a square invertible matrix.
## Therefore, we have to assume that every matrix value we supplied is always invertible.

## How to use these functions

## m <- matrix(1:4, nrow = 2, nrow = 2, byrow = TRUE)  # create a 2 x 2 matrix
## cachem <- makeCacheMatrix(m)     # cache the matrix
## cacheSolve(cachem)


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'\
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinv(inv)
    inv
}
