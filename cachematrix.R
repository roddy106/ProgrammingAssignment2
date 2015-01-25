## Title: Cachematrix
## Objective: To provide a fast matrix inverse function using caching
## Date started: 23 Jan 2015
## Author: Randeep Grewal

## Description: makeCacheMatrix
## The role of this function is to create a special matrix object that
## caches the inverse

makeCacheMatrix <- function(x = matrix()) {
## So this function has four 'methods'
## These are:
##      'set' which stores the original input matrix
##      'get' which returns the original matrix
##      'setinverse' which stores ('caches') the inverse matrix 
##      'getinverse' which returns the inverse matrix    
    
## m is variable in the environment of the makeCacheMatrix which will eventually
## contain the inverse matrix but we need to initialise it to NULL
    
    m <- NULL
    
    ## We are going to call this function to set the input matrix
    ## 
    set <- function(y){
        x <<- y
    
        ## since we can call 'set' from outside the makeCacheMatrix function
        ## we need to ensure that m is set to NULL when we get a new matrix
        ## that we put into x
        m <<- NULL
    }
    
    ## So the 'get' method simply returns to us the input matrix
    get <- function() x
    
    ## The 'setinverse' method is called to set the inverse matrix
    setinverse <- function(inverse) m <<- inverse
    
    ## The 'getinverse' method simply returns our inverse matrix m
    getinverse <- function() m
    
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## Description: cacheSolve
## The role of this function is to:
## On the first run:
##      (1) Compute the inverse matrix
##      (2) Store the inverse ie cache it using makeCacheMatrix$setinverse
##      (3) Return the inverse to the user
## On subsequent runs:
##      (1) Use makeCacheMatrix$getinverse to retrieve the cached inverse
##      (2) Return to user

cacheSolve <- function(x, ...) {
   
    ## So first we get what we hope is the cached inverse matrix and store it
    ## in the variable m
    m <- x$getinverse()
    
    ## however we need to check if it actually contains any values
    if(!is.null(m)){
        
        ## if it does then we say that we are returnign the cached matrix
        message("Getting cached matrix")
        
        ## and we then return it
        return(m)
    }
    
    ## if however the variable m is equal to null
    ## we better get the original matrix 
    ## we assign this to the variable data
    data <- x$get()
    
    ## we then solve the inverse and assign it to m
    m <- solve(data)
   
    ## we now need to cache this using the 'setinverse' method
    x$setinverse(m)
    
    ## and finally return it to the user
    m

}
