## The code below allows the user to cache the results of a
## matrix inversion.
## Usage is:
##  1. call makeCacheMatrx with the matrix you
##      want to invert.  It returns a vector that
##      contains the four functions to set and access
##      the matrix and its inverse.  A better name for
##      this function would be makeCachedMatrixFunctionVector
##
##  2. call cacheSolve with the vector of four functions
##      as the parameter. This will either create the
##      inverse of the matrix or return the cached version
##      if it has already been computed

## Given the target matrix, this function creates a vector
## of functions that can be used to access cached versions
## of the matrix and its inverse

makeCacheMatrix <- function(matrixInQuestion = matrix()) {
        inverseOfMatrix <- NULL
        set <- function(y) {
            matrixInQuestion <<- y
            inverseOfMatrix <<- NULL
        }
        get <- function() matrixInQuestion
        setInverse <- function(inv) inverseOfMatrix <<- inv
        getInverse <- function() inverseOfMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Given a list of access functions, this function will
## return the inverted matrix - either computed, or from
## a cached version, if it exists.

cacheSolve <- function(functionList, ...) {
        ## Return a matrix that is the inverse of the
        ## matrix specified when the makeMatrixCacheVector
        ## was called
    
        inv <- functionList$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        message("computing inverse")
        data <- functionList$get()
        inv <- solve(data, ...)
        functionList$setInverse(inv)
        inv
}

