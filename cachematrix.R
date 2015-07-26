## The function cacheSolve takes as parameter a special "matrix" object
## created by the function makeCacheMatrix, inverses it or uses the 
## cached value if it exists. 

## This function takes a matrix as parameter and returns
## a list object (special "matrix") of functions that 
## - set the matrix value (set)
## - get the matrix value (get)
## - set the inverse matrix value (setinvers)
## - get the inverse matrix value (getinvers)
makeCacheMatrix <- function(x = matrix()) {

	# initialise the cache variable
	m <- NULL

	# function to set a new vector and reset the cache variable
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
  
	# function to return the original vector
	get <- function() x

	# function to set the inverse matrix in the cache
	setinvers <- function(solve) m <<- solve

	# function to return the cached matrix
	getinvers <- function() m

	# list object containing the above defined functions
	list(set = set, get = get,
		setinvers = setinvers,
		getinvers = getinvers)

}


## This function takes as parameter the special "matrix"
## created by makeCacheMatrix, calculates the inverse of 
## that matrix, caches it and returns it. If the inverse
## has been calculated previously the cached value will be 
## returned.
cacheSolve <- function(x, ...) {
	# get the inverse of x
	m <- x$getinvers()

	# if there is a cached value display message and 
	# return cached value
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	# there was no cached value, so get the (actual) matrix
	data <- x$get()

	# calculate the inverse
	m <- solve(data, ...)

	# cache the inverse matrix
	x$setinvers(m)

	# return the (now) cached inverse matrix
	m	
}
