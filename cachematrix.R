## makeCacheMatrix() and cacheSolve() work together to calculate the inverse of an
## invertible square matrix and cache the inverse so that the inverse need only be
## calculated once.

## makeCacheMatrix takes a square matrix as input and returns a list of four functions:
## 1. set(y) assigns a new matrix to 'x'
## 2. get() returns the value of 'x'
## 3. setinv(inverse) sets the value of 'inv' to some value 'inverse'
## 4. getinv() returns the value of 'inv'

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve() first checks to see if the inverse of a matrix has already been calculated and cached.
## If it has, then the function returns the value of the cached inverse. If it hasn't, then
## the inverse of the matrix is calculated, cached, and printed to the console.

cacheSolve <- function(x, ...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}





