## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## x is the matrix to be inverted
    ## passed in when the a new "CacheMatrix" object
    ## is instantiated with makeCacheMatrix

    ## m will hold the inverse matrix
    m <- NULL
    
    ## set:
    ## store the matrix into "cached" x and make
    ## sure the cached inverse is cleared
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## get:
    ## return the original matrix
    get <- function() x
    
    ## setinverse:
    ## invert the matrix passed in and "caches" it in m
    setinverse <- function(matrix, ...) m <<- solve(matrix,...)
    
    ## getinverse:
    ## return the inverse matrix
    getinverse <- function() m
    
    ## return a list of of function calls for getting/setting
    ## the matrix and its inverse
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    mi <- x$getinverse()
    
    if(is.null(mi)) {
      data <- x$get()
      mi <- x$setinverse(data, ...) 
    }
    else {
      message("getting cached data")
    }

    mi
}
