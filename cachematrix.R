## These functions calculate and cache the inverse
## of a matrix

## This function caches the inverse matrix and
## acts as a special vector
makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function solves the inverse matrix
## We assume the given matrix is square and
## invertible
cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    
    ## Return a matrix that is the inverse of 'x'
    im
}


## I tested the functions with the following:
## > A <- matrix(c(0,1,1,1), nrow = 2, ncol = 2)
## > iA <- makeCacheMatrix(A)
## > cacheSolve(iA)
##       [,1] [,2]
## [1,]   -1    1
## [2,]    1    0
## > cacheSolve(iA)
## getting cached data
##       [,1] [,2]
## [1,]   -1    1
## [2,]    1    0
