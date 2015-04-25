## The pair of functions defined below (makeCacheMatrix and cacheSolve) avoid 
## redundant calculations of matrix inverses. They achieve this by storing 
## computed inverse values in a cache. Inverses are only calculated if they 
## are not found in the cache associated with these functions. Each
## time a new inverse is computed, it is added to the cache.

## function, makeCacheMatrix creates a special "matrix", which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list (inv = inv, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
    
}


## function, cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data)
    
    x$setInverse(inv) 
    
    inv
    
}
