## These 2 functions are used to store the results of a matrix
## inversion, so that this result can be reused, rather than
## having to recalculate the result, if needed.

##################################################
## This function is a list containing a function to:
##   set        - sets the original matrix
##   get        - gets the original matrix
##   setinverse - sets the solution to the matrix inverse
##   getinverse - gets the inverse of the matrix
## The results of these functions are used in cachesolve
##################################################
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##################################################
## This function looks for the cached result of the matrix.
## If found, it returns this result.  If not, it solves the
## matrix inverse.
##################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
}
