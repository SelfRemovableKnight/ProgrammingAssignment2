## makeCacheMatrix creates a CacheMatrix object. This is a "container" object 
## in which a matrix and a cached value of its inverse can be stored.
## Four functions are defined on the container to inspect and manipulate it
## * set() stores a matrix in the container (this clears the cached inverse, 
##   since that inverse belonged to a previously stored matrix)
## * get() obtains the stored matrix
## * getinverse() obtains the cached inverse (or NULL if not yet cached)
## * setinverse() stores a value as cached inverse
##   (intended use is that this value is the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
	cachedInv <- NULL
        set <- function(y) {
                x <<- y
                cachedInv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedInv <<- inverse
        getinverse <- function() cachedInv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that works on a CacheMatrix container; 
## it returns the inverse of the matrix stored in the container (if any):
##  if the CacheMatrix has a cached inverse, that value is returned; 
##  otherwise (no inverse cached) the inverse is calculated, cached for future 
##  retrieval, and returned  

## this function assumes that the provided matrix is invertible

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


