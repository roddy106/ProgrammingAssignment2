## Title: Cachematrix
## Objective: To provide a fast matrix inverse function using caching
## Date started: 23 Jan 2015
## Author: Randeep Grewal

## Description: makeCacheMatrix
## The role of this function is to create a special matrix object that
## caches the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve(inverse)
    getmatrix <- function() m
    list(set = set, get = get,
        setinverse = setinverser,
        getmatrix = getmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x%getmatrix()
    if(!is.null(m)){
        message("Getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m

}
