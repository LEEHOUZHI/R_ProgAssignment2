## Matrix Inversion Calculation usually requires alot of computation.
## As such in order to avoid repeated calculations of the same Matrix Inversion,
## cache is used to return previous saved values. 

## The makeCacheMatrix creates an object (which is really a list) containing a function to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { ## create matrix object x and sub-functions
        

        ## define cache invmat
        invmat <- NULL
        set <- function(y) {
                x <<- y ## assign the input of matrix y 
                        ## into variable x of the initial environment
                invmat <<- NULL ## re-initialize invmat into the initial environment to null
        }
        get <- function() x ## return matrix x
        setinverse <- function(inverse) invmat <<- inverse      ## set the cache invmat equal
                                                                ## to the inverse of the matrix x
        getinverse <- function() invmat ## return cached of inverse of x
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}





## The following function returns the inverse of the matrix. However, it first checks if
## the inverse has already been calculated. If so, it gets the result and skips the
## calculatoin. Otherwise, it cacculates the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invmat <- x$getinverse()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinverse(invmat)
        invmat
}