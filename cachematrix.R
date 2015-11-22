## These functions work together to cache the inverse of a matrix

## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## "i" holds the cached inverse
    i <- NULL
    
    ## Store the data and NULLify the cached inverse,
    ## since the data has changed
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Return the data
    get <- function() x
    
    ## Store the inverse
    setinverse <- function(inverse) i <<- inverse
    
    ## Return the inverse
    getinverse <- function() i
    
    ## Return a list containing the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then this retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check to see if the inverse is cached
    i <- x$getinverse()
    if(!is.null(i)) {
        ## Since it is, return the cached inverse, ending
        ## execution of the function
        message("getting cached data")
        return(i)
    }
    
    ## If we get here, the cache is empty, so compute the inverse
    data <- x$get()
    i <- solve(data, ...)
    
    ## Cache the inverse
    x$setinverse(i)
    
    ## Return the now cached inverse
    i
}
