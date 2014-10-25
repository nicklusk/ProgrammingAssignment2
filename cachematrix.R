## Functions for making and manipulating inverted matrices with caching

## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets 
## the cached values

makeCacheMatrix <- function(matrix.one = matrix()) {
        
        # Do I have a matrix?
        if (!is.matrix(matrix.one)) {
                stop("You don't have a matrix")
        }
        ## Let's initialize the inverse
        matrix.inv <- NULL
        ## Let's set the matrix
        set <- function(y) {
                matrix.one <<- y
                matrix.inv <<- NULL
        }
        
        # Let's grab and set the cached inverse of the  matrix using solve()
        get <- function() matrix.one
        set.inv <- function(solve) matrix.inv <<- solve
        get.inv <- function() matrix.inv
        ## Let's return a list of all the methods we just made
        list(
                set = set, 
                get = get,
                set.inv = set.inv,
                get.inv = get.inv)  
}

## Computes the inverse of the matrix returned by makeCacheMatrix()
## If the inverse has not already been set it sets it for you

cacheSolve <- function(matrix.cached, ...) {
        matrix.inv <- matrix.cached$get.inv()
        # Do I have a matrix?
        if(!is.null(matrix.inv)) {
                message("Grabbing matrix cache inverse")
                return(matrix.inv)
        }
        # Let's create an inverted matrix
        matrix.two <- matrix.cached$get()
        matrix.inv <- solve(matrix.two)
        matrix.cached$set.inv(matrix.inv)
        matrix.inv 
}