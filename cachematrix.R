## Utility functions to calculate the inverse of an Matrix. The functions defined can
## help reduce the costly computation associated with Inverse using caching rather 
## than computing it repeatedly.
## Usage: t <- makeCacheMatrix(matrix()) 
##        cacheSolve(t)
##

## This function creates a special "matrix" object that can cache its inverse.

## input <-- A sqaure invertible matrix object 
## output --> Accessor methods to store/access the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## store the matrix inverse
        inverse <- NULL
        ## store the matrix
        set <- function(newMatrix) {
                x <<- newMatrix
                inverse <<- NULL
        }
        ## grab the matrix
        get <- function() x
        
        ## store the Matrix inverse
        setInverse <- function(invMatrix) inverse <<- invMatrix
        
        ## access the stored Matrix inverse
        getInverse <- function() inverse
        
        ## list of accessor methods
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve will retrieve the inverse from the cache. 

## Input: vector object that was returned by the makeCacheMatrix 
## Output: inverse of the Original matrix 
cacheSolve <- function(x, ...) {
        ##lets verify that this is not a pure matrix object but a special matrix vector
        if(is.matrix(x)) {
                message("input should be a vector created using makeCacheMatrix")
                return(NULL)
        }
        
	inverse <- x$getInverse()
	## Verify if this object was already accessed and already has its inverse?
	if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
	}
	
	## Being accessed for the 1st time, so :
	## Calculate inverse, store the inverse for next time and then return the computed inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
