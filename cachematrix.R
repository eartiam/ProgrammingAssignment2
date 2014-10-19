## Coursera, R Programming: Assignment 2
## Compute the inverse of a matrix and cache it to avoid re-computation


## Create a matrix object that can cache its inverse
## Arguments:
##  x is an invertible matrix
## Returns:
##  a list with setters and getters for both the original matrix and its
##  inverse.
## Note: 'getinv()' will return NULL unless the inverse has been explicitly
##  set (e.g. by using 'cacheSolve()'

makeCacheMatrix <- function(x = matrix()) {
	# 'i' will contain the inverse of the matrix
	i <- NULL

	# set the value of the matrix, and clear the cached inverse (if any)
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	# get the value of the matrix
	get <- function() {
		x
	}

	# set the inverse of the matrix
	setinv <- function(inv) {
		i <<- inv
	}

	# get the inverse of the matrix
	getinv <- function() {
		i
	}

	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Compute the inverse of the matrix in a CacheMatrix object and store
## it for future use, or retrieve it from the cache, if already computed
## Arguments:
##  x is an object created with makeCacheMatrix
##  ... are additional parameters to be passed to the solve function
## Return:
##  The inverse of the matrix stored in the x argument

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	# Get from cache, if already computed
	xinv <- x$getinv()
	if (!is.null(xinv)) {
		message("getting cached data")
		return(xinv)
	}

	# Not cached: compute, store and return
	data <- x$get()
	xinv <- solve(data, ...)
	x$setinv(xinv)
	xinv
}

