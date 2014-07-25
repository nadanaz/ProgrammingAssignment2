## The makeCacheMatrix function creates a special matrix
## that can get, set matrix and get, set the inverse of
## that matrix.
## The cacheSolve function will return the inverse of the input matrix.
## If the inverse already calculated, get it from the cached
## or compute it otherwise.

## This function create a special vector that can
## get, set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()){
    ## Init the inverse
    inv <- NULL
    
    ## set function
    set <- function(y){
        ## if we set the same matrix again
        ## we don't have to recalculate
        ## we keep the old value
        if(!identical(x, y)){
            x <<- y
            inv <<- NULL
        }
    }
    ## get function
    get <- function() x
    
    ## set inverse function
    setinv <- function(inverse) inv <<- inverse
    
    ## get inverse function
    getinv <- function() inv
    
    ## Return a list the contain the set, get functions
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## This function calculate the inverse of the input matrix
## If the inverse already calculated, getting it from cached
cacheSolve <- function(x, ...) {
    
    ## Retrive the inverse of x
    inv <- x$getinv()
    
    ## If x already had the inverse, getting it from cached
    ## Skip computation
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ## If not, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Store the inverse of x
    x$setinv(inv)
    
    ##Return the inverse
    inv
}
