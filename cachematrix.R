## functions do create a matrix from input matrix and make it to an inverse matrix of the original

### SOLUTION 1
makeCacheMatrix <- function(x = matrix()) { # This function defines the input matrix but not in runtime
    i <- NULL
    set <- function (y) { 
        x <<- y #Assign the input matrix to store
        i <<- NULL #Assign null value to i because inverse matrix has not been calculated
    }
    get <- function() x
    setSolve <- function(solve) i <<- solve
    getSolve <- function() i
    #Define the function to get and set inverse value of the matrix
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}
## Function below calculate the inverse matrix if the inverse matrix has not been calculated
cacheSolve <- function(x, ...) {
    i <- x$getSolve() #Retrive the store value of the input matrix
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
        #Display the message and return value of the inverse matrix when it has already been calculated
    }
    data <- x$get()
    i <- solve(data, ...) #function 'solve'is used to calculate an inverse of the matrix
    x$setSolve(i)
    i #Display the value of the inverse matrix that was calculated
}

## Run test
my_matrix <- matrix(c(3,-4,2,-5), 2, 2)
inv <- makeCacheMatrix(my_matrix)
cacheSolve(inv)

#=======================
### SOLUTION 2 (shorter)
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
my_matrix <- matrix(c(3,-4,2,-5), 2, 2)
inv <- makeCacheMatrix(my_matrix)
cacheSolve(inv)