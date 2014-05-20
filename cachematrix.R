## There are two functions in this program.
## 1) makeCacheMatrix is used to initialize a cache from a matrix inversion.
## 2) cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix


## makeCacheMatrix creates a matrix object that cache its inverse.
## It sets and gets the matrix.  It also sets and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL 
        
        ## replace the underlying matrix
        set <- function(y) {
                
                ## set the value of the matrix
                x <<- y
                
                ## change in the matrix and cache of the inverse will be invalidated
                inv <<- NULL
        }
        
        ## returns underlying matrix
        get <- function() x
        
        ## store cache inverse
        setInverse <- function(inverse) inv <<- inverse
        
        ## get the inverse
        getInverse <- function() inv
        
        ## return a list to be used in the call functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)    
}


## cacheSolve takes in a list from makeCacheMatrix and returns the inverse.
## It checks if the inverse is calculated.  If so, it gets the invserse from 
## the cache.  Else, it will calculate the inverse of the matrix and set a value
## of the inverse in the cache.

cacheSolve <- function(x, ...) {
        
        ## get the cached value
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                
                ## cache hit
                message("getting cached data")
                return(inv)
        }
        
        ## not cached then calcuate the inverse
        data <- x$get()
        
        # compute the inverse
        inv <- solve(data, ...)
        
        # cache the inverse
        x$setInverse(inv)
        
        # return inverse
        inv
}
