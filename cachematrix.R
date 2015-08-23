## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is simply a function that generates an 'object'
## which is really a vector that stores a particular matrix as well
## as a set of getter/setter functions.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set<- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x

    setInverse <- function(inv) m <<- inv
    
    getInverse <- function() m

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Analogous to the example provided in the assignment prompt,
## this function simply checks if the inverse of the input matrix
## has been cached or not. If yes, it returns the stored value.
## Otherwise, it returns the computed value after caching it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}