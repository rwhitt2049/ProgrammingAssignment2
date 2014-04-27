## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Input an invertable matrix. The output will be a list of functions to get
# and set the inverse of the matrix using the solve() functions.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    # Return a list of 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# This function will search to see if the cached inverse of the matrix has 
# been calculated. If it has, it will use the cached data. If it hasn't, 
# cachesolve will solve the inverse of the matrix and return it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # If the cached inverse matrix is available, get it.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # If the cached inverse matrix doesn't exist, solve for it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
