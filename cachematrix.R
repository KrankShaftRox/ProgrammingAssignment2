## The below function creates a special object that can cache its inverse.

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

## The below function is used to cache the inverse of a matrix that is passed to it as argument.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
