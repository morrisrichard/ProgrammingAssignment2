## makeCacheMatrix
# This function creates a special matrix that can cache its inverse.

# Given an input matrix y, it creates an object x identical to y.
# It creates an empty object inv to contain the inverse of x. 
# It then creates a list of functions to set and then get x and the inverse.

## In order to run the function:
# 1. Create a matrix y: e.g y <- matrix(1:4, 2, 2)
# 2. Then run: x <- makeCacheMatrix(y)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
# This function computes the inverse of the matrix returned by makeCacheMatrix.
# It first checks if the inverse has already been calculated).
# If so, then it tells us it is "getting cached inverse.
# Otherwise, it computes and returns the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## In order to run the function, type: cacheSolve(x)