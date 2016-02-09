## Coursera R Programming Course Assignment #2
## Lexical Scoping
## 
## Author: Robert Begg Feb 8, 2016
## 
## Demonstrate using R lexical scoping rules by caching
## costly matrix operations

## makeCacheMatrix creates an object that stores a 
## matrix and can cache the inverse of the matrix
## Input x - a matrix than can be invered
## Return
##      List:
##      set()    sets the value of the original matrix x
##      get()    return the value of the original matrix x
##      setInv() stores the inverse of x
##      getInv() returns the inverse of x

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInv <- function(inv) invMatrix <<- inv
        getInv <- function() invMatrix
        return( list(
                set = set,
                get = get,
                setInv = setInv,
                getInv = getInv))
        
}


## cachesolve uses the object returned by makeCacheMatrix to
## return the inverse of the matrix x
## The first time the function is called the inverse is calculated
## subsequant calls will return the 
## input:
##      x - object returned by makeCacheMatrix
##      

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInv()
        if( !is.null(invMatrix)) {
                return(invMatrix)
        }
        m <- x$get()
        invMatrix <- solve(m, ...)
        x$setInv(invMatrix)
        
        return(invMatrix)
        
}
