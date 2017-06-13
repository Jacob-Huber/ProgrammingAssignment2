## Put comments here that give an overall description of what your
## functions do

##This prgoram is designed to take a matrix and find its inverse
##It must first check to see if it has it cached.

## Write a short comment describing this function
##makeCacheMatrix checks to see if there is a matrix cached in memory
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m<<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Write a short comment describing this function
##cacheSolve is designed to return the inverse matrix of the  input x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached matrix")
    }
    matrix <- x$get()
    m <- solve(matrix) %*% x
    x$setmatrix(m)
    m
}
