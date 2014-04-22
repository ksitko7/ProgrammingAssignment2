## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing
## The following function first sets the value of a matrix.
## It then sets the values for the martix or gets it, then sets
## the value of the inverse matrix or gets it

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of the special matrix
## that was created above, but first it checks to see if it 
## has already been calculated. If it finds that it has it uses
## the cached value and skips computation. If it doesn't fin it
## it caluclates it.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$inverse(m)
    m
}
