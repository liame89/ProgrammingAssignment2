## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix" to: 
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #initialise matrix to null, first time.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data") #if m exists, return cache
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) #if in this section of code, m doesn't exist, therefore solve for m.
    x$setinverse(m)
    m
}
