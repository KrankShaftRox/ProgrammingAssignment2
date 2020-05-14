## With these R functions we can efficiently calculate the inverse of a matrix
## If a given matrix does not change and we need to call its inverse
## repeatedly, for example in a loop, it is more efficient to store the 
## inverse matrix in cache rather than to recompute it every time.

## The function makeCacheMatrix creates a special matrix, or more precisely a 
## list of functions that: set and get the value of a matrix and 
## set and get its inverse matrix.

makeCacheMatrix <- function(x = matrix()) { ## argument with default mode of "matrix"
    inv <- NULL                             ## initialize inv as NULL 
    set <- function(y) {                    ## define set function 
        x <<- y                             ## value in parent environment
        inv <<- NULL                        ## reset new matrix inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument

    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator
}

## The cacheSolve function calculates the inverse of the matrix created using
## the makeCacheMatrix function above. The main purpose of the function is to 
## be more efficient when we need to calculate the inverse matrix repeatedly. 
## The cacheSolve function first checks whether the inverse matrix has already
## been calculated - in this case, the inverse matrix is retrieved from cache
## rather than calculated again. If it is the first time that the inverse is
## being calculated, the function calculates the inverse matrix and stores
## that inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()               ## Assign a value to the inv variable
    if(!is.null(inv)) {                 ## Conditinoal if statement
        return(inv)                     ## Return inv if it is not NULL
    }
    data <- x$get()                     ## Assign value to data variable
    inv <- solve(data, ...)             ## Apply inverse solve() function
    x$setinverse(inv)
    inv                                 ## Return inv
}
