# RProgramming course - Assignment 2
# The functions in this file implement a matrix object with support for caching
# the inverted matrix
# This means that if the matrix is used in contexts that require the inverted
# matrix, and the inversion is called many times, we can use this object for
# handling the matrix, and the inversion would only be calculated once - on
# the first time that the function for getting the inverted matrix is called -
# on subsequent calls, the cached value will be returned.

# The makeCacheMatrix function creates our matrix-object-with-cachable-invert
# It then returns a list with 4 functions:
# get - for getting the original matrix
# set - for assigning the original matrix. This has the side-effect of clearing
#    the cache
# getInvertedMatrix - for getting the inverted matrix from the cache
# setInvertedMatrix - for assigning a value to the cache

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL

        # set function - assign a new matrix to our object and reset
        # the cache, since any previous cached value is not relevant
        # anymore
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }

        # get function - returns the matrix stored in our object
        get <- function() x

        # setInverse - set the cache value for the inverse of the matrix
        setInverse <- function(inverse) cachedInverse <<- inverse

        # getInverse - get the cached value of the inverse of the matrix
        #   as previously stored by setInverse
        getInverse <- function() cachedInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# The cacheSolve function gets the inverse of the matrix stored by 'x'
# If the inverse was already solved and cached in 'x', then it is returned
# from the cache, otherwise, it is computed and stored in the cache and then
# returned

cacheSolve <- function(x, ...) {
        # Get the cached value and check if it was already set (if it was
        #   then it wouldn't be NULL 
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
            # If the inverse is already cached, return the cached value
            message("getting cached data")
            return(inverse)
        }

        # Inverse was not cached before. Get the data and calculate the inverse
        data <- x$get()
        inverse <- solve(data, ...)

        # Store the inverse in the cache
        x$setInverse(inverse)

        # return the inverse
        inverse
}
