## "makeCacheMatrix" creates a special matrix (or a list), containing four functions:
##  - set : set the value of the matrix
##  - get : get the value of the matrix
##  - setsolve : set the value of the inverse of the matrix
##  - getsolve : get the value of the inverse of the matrix

makeCacheMatrix <- function(m = matrix()){
    inv <- NULL
    set <- function(y){
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setsolve <- function(solve) inv <- solve
    getsolve <- function() inv
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## "cacheSolve" calculates the inverse of the special matrix created in "makeCacheMatrix" function.
## If the inverse has been calculated before it just uses the pre-calculated value from cache.
cacheSolve <- function(m,...){
    inv <- m$getsolve()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setsolve(inv)
    inv
}

