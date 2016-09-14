## This pair of functions allow one to calculate the inverse of a matrix using 
## R scoping rules to save processing time.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverseM <- function(inverse) i <<- inverse
    getInverseM <- function() i
    list(set = set, get = get, setInverseM = setInverseM, getInverseM = getInverseM)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    i <- x$getInverseM()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mtx <- x$get()
    i <- solve(mtx, ...)
    x$setInverseM(i)
    i
}
