## makeCacheMatrix() creates the special "matrix" object.
## cacheSolve() is used to get the inverse. If the inverse exists,
## then it gets the inverse from the cache.

## makeCacheMatrix function creates a special "matrix" object with
## 4 functions. Get() is used to return the matrix. Set() is used to update 
## the new matrix. Similarly getInverse() returns the inverse of the matrix
## and setInverse() is used to update the inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrixInverse <<- inverse
        getInverse <- function() matrixInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve is a function written to return the inverse of the special
## matrix object. The function calls the getInverse() on the matrix object.
## If the inverse already exixts then the it returns the already calculated inverse.
## If not, then it calculates the inverse and returns it, at the same time it assigns the inverse
## using the setInverse() function.

cacheSolve <- function(x, ...){
	inverse <- x$getInverse();
	if(!is.null(inverse)){
		message("Inverse already existing... getting cached data");
		return(inverse);
	}
	inverse <- solve(x$get());
	x$setInverse(inverse);
	inverse;
}
