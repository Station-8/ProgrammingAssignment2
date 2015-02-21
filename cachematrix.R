## Put comments here that give an overall description of what your
## functions do

## The below function creates a "special vector" containing a list functions to set
## the value of the vector, get the value of the vector, set the value of the

source("cachematrix.R")

## Create matrix caching function which takes matrix x as its argument:
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Initializes m, which will be the inverse matrix, to null.
        set <- function(y) {
                ## set function used if matrix is changes. x is
                ## assigned to the changed matrix
                x <<- y
                m <<- NULL
                ## initializes m to NULL in cases where original
                ## matrix was changed.
        }
        ## get is a function, returning matrix x:
        get <- function() x
        ## setinverse is function assigning m to inverse calculated in the
        ## cacheSolve function.
        setinverse <- function(inverse) m <<- inverse
        ## getinverse is function returning m
        getinverse <- function() m
        ## Create list of functions and assign above functions to them:
        list(set = set, get = get, setinverse = setinverse,
                getinverse = getinverse)
        }

## The below function computes the inverse of the special "matrix" returned by
## makeCacheMatrix, above. If the inverse has already been calculated, and
## the matrix has not changed, the cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## The first time m is evaluated, it will be null because getinverse
        ## is null. The inverse has yet to be created.  The next time the
        ## function is run with same beginning matrix, getinverse will not
        ## be null:
        m <- x$getinverse()
                if(!is.null(m)) {
                ## If m is not null, then it has already been calculated and
                ## cached, and "getting cached data" message is returned:       
                message("getting cached data")
                ## and the cached inverse matrix is returned:         
                return(m)
        }
        ## Only if cache is empty, the inverse of the matrix is calculated.
    
        ## data matrix variable is assigned to function get, which is original
        ## matrix x
        data <- x$get()
        ## The inverse of data (matrix x) is calculated:
        m <- solve(data, ...)
        ## the setinverse value is set for the makeCacheMatrix function.
        x$setinverse(m)
        ## the inverse matrix is returned:
        m
        }