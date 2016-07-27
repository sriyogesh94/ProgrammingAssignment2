## Functions that cahce the inverse of a matrix
## set() - set the value of the matrix
## get() - get the value of the matrix
## setInverse - set the value of the inverse matrix
## getInverse - get the value of the inverse matrix

## Create a special "matrix", which is a list containing,
## set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        
        x <<- y
        
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Reuse the cached inverse or calculate the inverse of the 
## special "matrix" created above

cacheSolve <- function(x, ...) {
    
    
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
    
        message("getting cached data")
        
        return(inv)
    }
    
    m1 <- x$get()
    
    inv <- solve(m1, ...)
    
    x$setInverse(inv)
    
    inv
}