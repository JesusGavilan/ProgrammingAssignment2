## Create the special vector with the content
## of the matrix and the method set,get,
## setinverse, getinverse

## Gettters and setters of the matrix
## Getters and setters of the inversed
## matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inver) i <<- inver
	getinverse <- function() i
	list( set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Get the matrix, get the inverse.
## In case was previouly calculated
## it returns the inverse stored.
## Otherwise it calculates the inverse
## with solve() function and stored the
## result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)){
		message("getting cached inverse matrix")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
