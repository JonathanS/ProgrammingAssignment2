# Coursera - R Programming - Programming Assignment 2
# Caching the Inverse of a Matrix

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

# Tested as follows:
# m1 <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
# m2 <- matrix(c(10, 9, 8, 7), nrow = 2, ncol = 2)
# cm <- makeCacheMatrix(m1)
# cacheSolve(cm) # doesn't use cache, returns solve(m1)
# cacheSolve(cm) # returns cached result
# cm$set(m2)
# cacheSolve(cm) # doesn't use cache, returns solve(m2)
# cacheSolve(cm) # returns cached result
