# makeCacheMatrix takes an invertible matrix x and creates getters and setters
# to compute, cache, store, and retrieve its inverse x^{-1} via solve()

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


# this is a cache-aware variant of solve(), which takes an enhanced matrix
# makeCacheMatrix and returns the inverse. It returns the cached value if
# present, and if not computes it "the hard way" and stores the result.

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

# _Usage example_
# Create a 2000*2000 matrix, solve takes ~2 seconds
# then add the caching functions
# first run of cacheSolve again takes ~2 seconds, but second run simply
# returns cached data and takes ~0 seconds

# > A <- matrix(runif(2000*2000), ncol=2000)
# > system.time(solve(A))
#    user  system elapsed
#   3.589   0.051   1.926
# > z <- makeCacheMatrix(A)
# > system.time(cacheSolve(z))
#    user  system elapsed
#   3.826   0.012   2.004
# > system.time(cacheSolve(z))
# getting cached data
#    user  system elapsed
#       0       0       0
# >