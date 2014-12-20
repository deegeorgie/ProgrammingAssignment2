## This function is meant to load the inverse of a matrix from cache
## or compute it from scratch if it has not yet been calculated...

## special matrix function that can cache its' inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## set the value of the matrix
        
        i<- NULL
        
        set <- function(y) {
                
                x <<- y
                
                i <<- NULL
        }
        ## get the value of the matrix
        
        get <- function() x
        
        ## set the inverse of the matrix
        
        setinverse <- function(solve) i <<- solve
        
        getinverse <- function() i
        
        ## get the inverse of the matrix
        
        list(set = set, get = get,
             
             setinverse = setinverse,
             
             getinverse = getinverse)
}


## function to return the inverse of the matrix from the 
## makeCacheMatrix function. If the inverse exists in cache,
## retrieve it.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse of the matrix  
        
        i <- x$getinverse()
        
        ## check if there is the matrix 
        
        if(!is.null(i)) {
                
                message("retrieving cached data")
                
                return(i)
        }
        ## if not: get the inverse of the matrix   
        
        data <- x$get()
        
        i <- solve(data, ...)
        
        ## set the inverse of the matrix 
        
        x$setinverse(i)
        
        i
}
