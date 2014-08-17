## R programming
## Assignment 2: Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()){
    
    xinv <- NULL
    
    set <- function(y){
        
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    
    setinv <- function(inv) xinv <<- inv
    
    getinv <- function() xinv
    
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}



## This function computes the inverse of the special "matrix".
## If the inverse has already been computed the function retrieve its value from the cache.

cacheSolve <- function(x, ...) {

    xinv <- x$getinv()
    
    if(!is.null(xinv)){
        message("getting data from the cache")
        return(xinv) 
    }
    
    data <- x$get()
    xinv <- solve(data)
    x$setinv(xinv)
    
    ## Returns a matrix that is the inverse of 'x'
    return(xinv)
}
