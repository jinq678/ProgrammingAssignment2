## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this functions initiates a special oject that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverseM) m <<- inverseM
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}



## Write a short comment describing this function
## this function refers back to the makeCacheMatrix function looking
# for a cached matrix inverse, if it doesn't exist cacheSolve
# will calculate the inverse of the matrix and cache it for future calls
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
