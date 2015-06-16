## This R Script contains functions to create a special matrix object 
## that can cache its inverse and other function that computes the 
## inverse of the matrix conditionally if the inverse is already
## not present in the cache.

## makeCacheMatrix function returns a special matrix object that can cache its inverse.
## As a input takes an invertible matrix so that its inverse can be computed.
## Return list object that contains functions to set, get , setInverse and getInverse 
## as seperate list elements.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(mean) inverse <<- mean
        getInverse <- function() inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function computes the inverse of the matrix passed as parameter 'x'.
## If the inverse is already present it displays the message "getting cached data"
## returns inverse from the cache else it computes the fresh inverse using 
## the solve() function and store it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getMatrix()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
