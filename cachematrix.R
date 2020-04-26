## The following function first sets the value of a "matrix".
## It then sets the values for the martix or gets it, then sets
## the value of the inverse matrix or gets it

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function calculates the inverse of the special matrix
## that was created above, but first it checks to see if it 
## has already been calculated. If it finds that it has it uses
## the cached value and skips computation. If it doesn't find it
## it caluclates it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    return(inv)
}
