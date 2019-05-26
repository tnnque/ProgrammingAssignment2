## functions do create a matrix from input matrix and make it to an inverse matrix of the original

## This function defines the input matrix but not in runtime
# Create an object 'makeCacheMatrix' to store matrix and inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    stored <- x
    inverse <- NULL
    
    list(stored = stored, inverse = inverse)
}

## Function below calculate the inverse matrix if the inverse matrix has not been calculated
cacheSolve <- function(y, ...) {
    if(!is.null(y$inverse)) {
        message("getting cached data")
        return(y$inverse)
    }
    else {
        y$inverse <- solve(y$stored)
    }
    y
}


## Test
my_matrix <- matrix(c(3,-4,2,-5), nrow=2, ncol=2)
inv <- makeCacheMatrix(x)
cacheSolve(inv)