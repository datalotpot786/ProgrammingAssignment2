## Matrix Inversion is generally a costly computation and sometimes 
## it is beneficial caching the inverse of a Matrix than computing again and again.
## Below 2 functions are created to implement the logic of caching

## Below function creates a list of function to get and set the 
## value of matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Below function computes the inverse of matrix if it is called first time
## For all subsequent calls it returns inverse matrix from Cache using is.null 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
