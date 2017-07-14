#This file contains functions to cache the inverse of a matrix, then return the inverse from the cache.

#The first function creates a special "matrix" object that can cache its inverse.
#It is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix())
{
    Inverse <- NULL
    set <- function(y)
    {
        x <<- y
        Inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function() Inverse <<- solve(x)
    getInverse <- function() Inverse
    list
    (
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

#This function returns a matrix that is the inverse of 'x' from the cache created in the previous function.
cacheSolve <- function(x, ...) 
{
    Inverse <- x$getInverse()
    if (!is.null(Inverse)) 
    {
        message("Reading Cache")
        return(Inverse)
    }
    data <- x$get()
    Inverse <- solve(data, ...)
    x$setInverse(Inverse)
    Inverse
}
