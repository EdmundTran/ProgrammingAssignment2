## Put comments here that give an overall description of what your
## functions do

# These functions return the inverse of a matrix. If the function is
# called again with a previously used matrix, it will retrieve the
# inverse from the cache rather than recomputing it, which saves
# computing resources.


## Write a short comment describing this function

# This function creates a list of 4 functions, which respectively,:
# 1.) set the value of the matrix
# 2.) get the value of the matrix
# 3.) set the value of the inverse
# 4.) get the value of the inverse

# Prerequisites:
# Only an invertible matrix can be used as the argument.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# This function returns the inverse using the list of functions
# created by makeCacheMatrix. First, it checks to see if the
# inverse has already been calculated. If it has, it simply
# returns the inverse from the cache. If it hasn't, it
# calculates the inverse, sets the value of the inverse in the
# cache, and then returns the inverse.

cacheSolve <- function(x, ...) {
        
        # Get inverse of x from cache
        i <- x$getinverse()
        
        # If there was an inverse in cache, print message
        # and return inverse
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # Otherwise, get the matrix,
        data <- x$get()
        
        # compute the inverse,
        i <- solve(data, ...)
        
        # put the inverse in cache,
        x$setinverse(i)
        
        # and return the inverse
        i
}
