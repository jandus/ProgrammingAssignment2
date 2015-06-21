## Functions on this program cache the inverse of a matrix. 
## This functions are used to improve performance on the code 
## when we need to inverse same matrix several times.


## makeCacheMatrix function creates a list of elements that can be 
## used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    ## Set the value of the matrix to be inverse
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s 
    list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## cacheSolve function inverse a matrix that is passed through 
## makeCacheMatrix. If the inverse was done previously, this 
## function returns the value on cache to improve permormance on the function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    ## Verify if the inverse of matrix is cached
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    # Get values of the matrix which will be inverted
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
