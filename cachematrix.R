## This file contains two functions - makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix generates a list of functions that can be used to 
## set and retrieve a matrix and its inverse.
##
## cacheSolve returns the inverse of a matrix from a cached copy (if
## the copy exists) or calculates it from scratch (if the copy does
## not exist).



## makeCacheMatrix takes as its input a matrix and returns a list of 
## the following functions:
##
## set - to set or reset a matrix, 
## get - to return that matrix,
## setinverse - to calculate the inverse of the matrix,
## getinverse - to return the inverse of the matrix.
##
## 
makeCacheMatrix <- function(x = matrix()) {
        cachedCopy <- NULL
        set <- function(y) {
                x <<- y
                cachedCopy <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedCopy <<- inverse
        getinverse <- function() cachedCopy
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}



## cacheSolve takes as its input a matrix and returns the inverse
## of that matrix. In order to improve performance, the inverse is
## held in, and retrieved from, a cached copy so that processing is
## not unnecessarily repeated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cachedCopy <- x$getinverse()
        if(!is.null(cachedCopy)) {
                message("Retrieving inverse from cache")
                return(cachedCopy)
        }
        originalMatrix <- x$get()
        cachedCopy <- solve(originalMatrix,...)
        x$setinverse(cachedCopy)
        cachedCopy
}
