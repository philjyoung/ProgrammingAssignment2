## Program contains 2 functions that will store a matrix and it's inverse.

## makeCachMatric stores a matrix and it's inverse in the parent environment.
## Each time a new matrix is stored the value of the inverse is set to null.
## Also creates the functions used to retreive the inverse which are used in the subsequent function. 


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function to return the inverse if cached, or calculate if not. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
