## Caching the Inverse of a Matrix

## This function creates
## a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	set <-function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x

	setInverse <- function(newInverse) inverse <<- newInverse
	getInverse <- function() inverse
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## This function computes
## the inverse of the special "matrix"
## returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()

	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)

	inverse
}
