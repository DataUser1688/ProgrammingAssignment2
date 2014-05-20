## There are two functions in this program.
## 1) makeCacheMatrix is used to initialize a cache from a matrix inversion.
## 2) cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix


## makeCacheMatrix creates a matrix object that cache its inverse.
## It sets and gets the matrix.  It also sets and gets the value of the inverse.

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


## cacheSolve takes in a list from makeCacheMatrix and returns the inverse.
## It checks if the inverse is calculated.  If so, it gets the invserse from 
## the cache.  Else, it will calculate the inverse of the matrix and set a value
## of the inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
