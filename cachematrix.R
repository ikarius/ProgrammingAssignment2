## makeCacheMatrix:
## parameter:   x is a matrix, or an empty one if no parameter
##              cache a matrix and its inverse
## return:      a list of functions to get/set the value of the matrix and 
##              its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Variable used to store the inverse of the matrix
    i <- NULL
    
    ## 'Setter' function: set the value of the cached matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## 'Getter' functio: get the cached value for the matrix
    get <- function() x
    
    ## Store the inverse of the matrix in a new environment
    setinv <- function(inverse)     
        i <<- inverse   
    
    ## Return the inverted matrix
    getinv <- function()        
        i
    
    ## Return a list containing all the functions
    ## used to cache a matrix and its inverse 
    list(set = set, 
         get = get, 
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve:
## parameter:   x, matrix to be inverted
## parameter:   ..., list of additional parameters to pass to the 'solve()' function
## return:      a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## 
    ## We assume that the matrix supplied is always invertible 
    
    i <- x$getinv()                     ## Get the cached value for the inverse of x "matrix" 
    
    if (!is.null(i)) {
        message("Cache hit")            ## Warns the user that a result was found in the cache
        i                               ## and returns it
    } 
    else {    
        x$setinv(solve(x$get(), ...))   ## If no cache for this matrix, compute invert matrix, store it
                                        ## The '...' are extra parameters first passed to 'cacheSolve()'
                                        ## and then to 'solve()' if needed.
        x$getinv()                      ## Finally return the inverse
    }
}