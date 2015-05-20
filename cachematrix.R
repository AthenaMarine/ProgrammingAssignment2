## Inverting matrices takes a long time to compute.
## These functions improve performance by caching
## the result of matrix inversion for later use by your program.

## Function to create a new cached matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # variable used to hold the cached result
    m <- NULL
    
    # function to set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # function to get the value of the matrix
    get <- function() x
    
    # function to set the inverse value of the matrix
    setinverse <- function(solve) m <<- solve
    
    # function to get the inverse value of the matrix
    getinverse <- function() m
    
    # create a list of these functions to access from your environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## returns the inverted values of the cached matrix you pass to it, (created from the makeCacheMatrix function above.)
## the result will either come from cache if the inversion was previously calculated or
## function will calculate and store the inversion into cache if no previous result exists.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## try to get the inverted matrix from cache
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## nothing in the cache, so calculate and store the inverse of the matrix provided.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
