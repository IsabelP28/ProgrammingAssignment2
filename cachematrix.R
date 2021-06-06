## This function, "makeCacheMatrix", will create a matrix that is invertible. Later, the function "cacheSolve" will compute the inverse of this matrix. 
## I will first name the function "makeMatrix" and transfer the successful function to "makeCacheMatrix" once done. 
##
makeMatrix <- function(x = matrix()) {
new <- NULL
parta <- function(y) {
	x <<- y
	new <<- NULL
	}
	partb <- function() {
		x
		}
		setInverse <- function(inverse) {
			new <<- inverse
			}
			getInverse <- function() {
				new
				}
				list(parta=parta,
				partb=partb,
				setInverse=setInverse,
				getInverse=getInverse)
}
makeCacheMatrix <- makeMatrix
somematrix <- makeCacheMatrix (matrix(1:4,2,2))
##this creates a 2x2 matrix with numbers 1:4, in order to test that the function was designed properly.
somematrix$partb()
##matrix was generated!


## cacheSolve takes the matrix produced and inverts it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        new <- x$getInverse()
        if(!is.null(new)) {
        	message("getting cached data")
        	return(new)
        	}
        	data <- x$partb()
        	new <- solve(data, ...)
        	x$setInverse(new)
        	new
}
cacheSolve(somematrix)
##this produces the inverse of the 2x2 matrix "somematrix"

