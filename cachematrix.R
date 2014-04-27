## These functions allow user to cache matrices' inverse in a special 
## object and then retrieve the inverse from the cache.



## The first function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

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
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m  <- x$getinverse()   ## makes a query to cached inverse
        if (!is.null(m)){
                message("getting cached data") 
                return(m) ## if cache exists function returns the cached inverse
        }   
        data  <- x$get() ## if there's no cache function computes the inverse and returns it
        i  <- solve(data, ...)
        x$setinverse(m)
        m   
}
