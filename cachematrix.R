## These functions are used together to efficiently find the
## inverse of a matrix (the inverse is calculated once and then
## cached).
## 
## Usage example:
##  
##    m <- matrix( c(3,4,2,5), 2, 2)    # create a 2x2 matrix
##    cm <- makeCacheMatrix( x )        # create a cacheMatrix object with the matrix
##    mi <- cacheSolve(cm)              # return the inverse matrix
##    mi <- cacheSolve(cm)              # return the inverse matrix (cached from the
##                                      #  first run
##

## makeCacheMatrix accepts a matrix parameter (assumed to be invertable) 
## it returns a list of functions:
##
##  set - sets the matrix and clears the inverse matrix
##  get - returns the matrix
##  setinverse - pass in the matrix and set (and return) the inverse matrix
##  getinverse - return the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

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


## cacheSolve acceps a CacheMatrix object (see above) and returns
## the inverse matrix.  If the inverse has been computed before
## it returns the cached value.
##
## extra parameters for the "solve" function can be added

cacheSolve <- function(x, ...) {
        
    ## get the inverse from the CacheMatrix object
    mi <- x$getinverse()
    
    ## if the inverse hasn't been set yet,
    if(is.null(mi)) {
      
        ## pass the matrix (x$get) into the setinverse
        ## function along with any extra parameters for
        ## the solve function
        mi <- x$setinverse(x$get(), ...)
        
    }
    else {
      
        ## the inverse has been set.  if you want to
        ## verify the cached data is working, uncomment the 
        ## following line (remove the "#").
      
        #message("getting cached data")

    }

    ## return the cached index
    mi
    
}
