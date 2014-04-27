## These functions allow user to cache matrices' inverse in a special 
## object and then retrieve the inverse from the cache.


## makeCacheMatrix creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m  <- NULL
        set  <- function(y){
                x <<- y
                m <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) m  <<- inverse
        getinverse  <- function() m
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        
}


## cacheSolve computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cach

cacheSolve <- function(x, ...) {
        m  <- x$getinverse()   
        if (!is.null(m)){
                message("getting cached data") 
                return(m)
        }   
        data  <- x$get()
        i  <- solve(data, ...)
        x$setinverse(m)
        m   
}
