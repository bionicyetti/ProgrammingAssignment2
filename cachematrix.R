## Two functions makeCacheMatrix and cacheSolve that get/set a matrix and computes an inverse matrix
## Functions assume that all matrices recieved are invertable

## Function that gets/sets a matrix in a cache and allows the matrix inverse to be called

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #variable that stores cache matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse 
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that solves input matrix for an inverse and feeds makeCacheMatrix
## If matrix inverse is already stored in cache the function returns the already computed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data(), ...)
        x$setInverse(inv)
        inv
}
