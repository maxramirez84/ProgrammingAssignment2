## Cache the inverse matrix so that when we need it again, it can be looked up 
## in the cache rather than recomputed.

## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to:
##    set the value of the matrix
##    get the value of the matrix
##    set the inverse matrix
##    get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse = matrix()) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse matrix created with the makeCacheMatrix 
## function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the 
## inverse matrix in the cache via the setinverse function.
## Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x = matrix(), ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
