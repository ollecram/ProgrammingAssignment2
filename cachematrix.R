## These two functions support caching the result of 
## computing the inverse of a matrix. 

## This function creates an object (list of four functions)
## holding a matrix and its inverse in a private environment
## The four functions exposed to the exploiters support the 
## reuse of the computed inverse (see cacheSolve)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function accepts in input the type of object created  
## by makeCacheMatrix. If the inv object is null the inverse  
## of the matrix associated to the input object is computed  
## and saved in the inv object, while on subsequente invocations 
## the saved inv object is returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    
}
