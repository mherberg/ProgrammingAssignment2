## a pair of functions that cache the inverse of a matrix

## `makeCacheMatrix`: This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(inverse) i <<- inverse
        geti <- function() i
        list(set = set, get = get,
             seti = seti,
             geti = geti)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        i <- x$geti()
        
        # checks if inverse already calculated
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # else calculates inverse
        data <- x$get()
        i <- solve(data, ...)
        
        # sets value of inverse in cache
        x$seti(i)
        
        ## Return a matrix that is the inverse of 'x'
        i
}
