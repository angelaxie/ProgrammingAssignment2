##This pair of functions cache the inverse of a matrix.

## This function stores a list of functions that aim to chache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m

    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function computes the inverse of matrix returned by makeCacheMatrix.
## If the inverse has already been calculated for the matrix, catchSovle retrieves the cached solution for the operation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
