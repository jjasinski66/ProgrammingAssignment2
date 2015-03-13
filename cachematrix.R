## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function..
##      1) sets the value of the square matrix
##      2) gets the value of the matrix
##      3) Sets the inverse of the matrix
##      4) gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
##
## This function checks if the inverse of the matrix has been cached, 
## if no, it calculates and caches the inverse, 
## if yes, it retrieves the cached inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
