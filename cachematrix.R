## The functions allow the inverse of a matrix to be computed and cached. 


## Creates an object that represents a matrix and
## that contains a cache of its inverse
makeCacheMatrix <- function(matM = matrix()) {
    matMInv <- NULL # Cached matrix inverse, initially set to NULL
    
    # Set matrix value
    set <- function(y) {
        matM <<- y
        matMInv <<- NULL # Reset cached matrix inverse to NULL
    }
    
    # Get matrix value
    get <- function() matM
    
    # Set matrix inverse value
    setInv <- function(inv) matMInv <<- inv
    
    # Get matrix inverse value
    getInv <- function() matMInv
    
    # Return the list of functions
    list(set = set, 
         get = get, 
         setInv = setInv, 
         getInv = getInv)
}


## This function returns the inverse of the matrix x.
## It uses a cached result if one is available, else, it computes 
## the inverse and stores the result in the cache before returning the result. 
cacheSolve <- function(x, ...) {
    
    # Attempt to get a cached inverse of the matrix.
    # If available, reutrn the cached result
    matInv <- x$getInv()
    
    if (!is.null(matInv)) {
        message("Returning cached result...")
        return(matInv)
    }
    
    # Else compute and cache inverse of the matrix x.
    # Get the matrix, solve for inverse, and cache result
    message("Computing result...")
    matM <- x$get() 
    matInv <- solve(matM)
    x$setInv(matInv)
    
    # Return computed inverse
    return(matInv)
}
