## These functions provide the user with the ability to cache a matrix, 
## calculae the inverse of that matrix and cache the calculated inverse

## The makeCacheMatrix function caches a matrix 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function() {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of a given matrix and
## caches the result

cacheSolve <- function(x, ...) {r$setinverse
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
