# Calcluation of inverse of a matrix is a cpu expensive task.
# the two function makeCacheMatrix and cacheSolve will calculate the
# inverse of a matrix one time and caches the result for a faster retrieval
# for the subsequent calls to cacheSolve. 
#
# makeCacheMatrix: Accepts a square matrix and initiates the matrix 
# inverse to null.
#
# Argument: a square matrix.
# Return Value: list of setter and getter functions that can operate on the
#               passed in matrix and the cached inverse of the matrix.
#
# Example: we could set the matrix after we have the makeCacheMatrix Object.
#
# > mx <- makeCacheMatrix()
#
# > mx$set( matrix(1:4, 2, 2) )
#
# > cacheSolve(mx)
#     will return the inverse of the matrix.
#
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getinverse <- function()m
        list(set = set, get = get,
             setsolve = setsolve,
             getinverse= getinverse)
}


## cacheSolve: will solve the inverse of a square matrix( if exists) and 
# assigns the value in a variable in a separate envronment than global
# environment. It does that by the use of getter and setter in the returned
# list from makeCacheMatrix function. Since the value of inverse variable is
# NULL, first time call to cacheSolve will cause the function to retrive
# the passed matrix and solve its inverse. For the subsequent calls to
# cacheSolve it does not calculate the inverse but it will retrive and
# returns the cached value makeCacheMatrix.
# NOTE: if the matrix gets reset by the use of set function in 
#       makeCacheMatrix, cacheSolve will recalculate the inverse for 
#       the new matrix.
# Return Value: Inverse of a Matrix
# Return class: matrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
