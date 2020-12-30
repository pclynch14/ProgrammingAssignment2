## These two functions cache the inverse of a matrix

## This first function creates matrix, can cache inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(matrix) {
                x <<- matrix
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This second function looks to see if inverse has already been calculated.
## If it has, and the matrix has remained unchanged, it returns the inverse
## from the cache. If not, or if the matrix has changed, it computes the inverse
## of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) %*% data
        x$setInverse(inv)
        inv
}
