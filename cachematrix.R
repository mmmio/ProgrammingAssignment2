## makeCacheMatrix has 4 functions
## 1. "set" function :Set the value of matrix
## 2. "get" function :Get the value of matrix
## 3. "setinverse" function :Set the matrix inverse
## 4. "getinverse# function :Get the matrix inverse
## caseSolve returns the matrix inverse

## This function creates matrix object that can cash its inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y){
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    minv <<- solve(x)
    setinverse <- function(m = x) minv <<- solve(m) 
    ## inverse <<- solve(x) 
    getinverse <- function() minv
    list(set=set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the matrix returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data)
    x$setinverse(data)
    minv
}
