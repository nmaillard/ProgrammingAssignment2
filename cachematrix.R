## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##keeps a cache object of the inversed matrix to serve it
makeCacheMatrix <- function(x = matrix()) {
	##initial inverse is null
	inversed <- NULL

        ##set the matrix and no inversed cache yet		
	set <- function(y) {
		x <<-y
		inversed <<- NULL
	}
	## get the matrix
	get <- function() x
	##set the inverse for the inversed cached matrix
	setinverse <- function(inverse) inversed <<-inverse
	##return inversed cached matrixed
	getinverse <- function() inversed
	
	##list available functions
	list(set = set,get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## Write a short comment describing this function
##returns the inverse of a matrix or it's cached inverse if it has allready been calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	##Suppose matrix is invertible
	##try to get inverse
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached inversed matrix")
		return(inverse)
	}
	##inverse not cached yet
	##
	data <- x$get()
	##calculate inverse
	inverse <- solve(data)
	##store it
	x$setinverse(inverse)
	inverse
}
