## Pair of functions to cache the calculation of a matrix inversion
## makeCacheMatrix() - creates a 'cacheable' matrix inversion
## cacheSolve() - calculates inversion and use cached result whenever possible

## Creates a Cache Matrix that can cache its inverse. Contains functions
## to set and get value of the matrix and set and get the inverse
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setsetinverse,
             getmean = getinverse)
}


## Computes the inverse of makeCacheMatrix. 
## If inverse already calculated, return value from cache
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
        m
}
