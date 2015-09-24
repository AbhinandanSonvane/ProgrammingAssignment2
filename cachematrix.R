# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
# The first function creates a list containing supportability functions
# The second function retrieves and set the inverse of the matrix in cache

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function returns the inverse of the matrix. It first checks to see if 
# the inverse is alreay computed and available. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# Input : An invertible matrix.
# Assumption : The input matrix is always invertible.
# Output : Inverse of the matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# Test Case 1
# > x = rbind(c(1, 2), c(2, 1))
# >  m = makeCacheMatrix(x)
# > m$get()
#     [,1] [,2]
# [1,]    1    2
# [2,]    2    1
# > cacheSolve(m)
#           [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
# > cacheSolve(m)
# getting cached inverse matrix.
#           [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333

# Test Case 2
# > x = rbind(c(5, -8), c(-8, 5))
# >  m = makeCacheMatrix(x)
# > m$get()
#     [,1] [,2]
# [1,]    5   -8
# [2,]   -8    5
# > cacheSolve(m)
#           [,1]       [,2]
# [1,] -0.1282051 -0.2051282
# [2,] -0.2051282 -0.1282051
# > cacheSolve(m)
# getting cached inverse matrix.
#           [,1]       [,2]
# [1,] -0.1282051 -0.2051282
# [2,] -0.2051282 -0.1282051