## The combined use of these functions allows to cache the result of matrix inversion (e.g. solve(matrix))
## This is useful as the solve() operation is expensive in terms of processing power. 
## With caching, the matrix inversion is only performed once. The cached version can be used several times
## without incurring in additional computational costs.


## The makeCacheMatrix function creates a list with the following functions:
## - set(matrix): specifies which matrix's inverse would be eventually cached
##				 (set to NULL the cached inverse matrix)
## - get(): get the matrix
## - setsolve(solve): set the inverse matrix
## - getsolve(): get the cached inverse matrix (NULL if still not cached)

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function cacheSolve(x, ...) takes as input the list created by the
## function makeCacheMatrix(matrix) and returns the inverse of the matrix.
## cacheSolve() checks if the matrix's inverse has been already computed. 
## In case, it returns the cached matrix's inverse;  otherwise, it computes
## the matrix's inverse with the function solve() using as extra parameters, 
## the extra parameters provided in the function (...). 
## Once the inverse of the matrix is computed, it is saved in the cache.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached solve")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s	
}
