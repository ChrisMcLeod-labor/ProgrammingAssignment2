## These functions work together to cache the inverse of a matrix
## rather than compute it repeatedly. cachesolve will
## calculate the inverse of the matrix or it will return an
## inverse that has already been calculated if the matrix is the
## same for each calculation.

## makeChacheMatrix creates an object that stores a matrix and its 
## inverse. It builds a set of functions (set, get, setmatrix, getmatrix)
## in a list in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cachesolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated, and the matrix not changed,
## cachesolve should retrieve the inverse from the cache.
## It works by populating and/or retriveing the inverse
## from the object created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix() 
        if(!is.null(m)) {  
                message("getting cached data")
                return(m) 
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}