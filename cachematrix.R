## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. The two functions shown here help in caching the
## inverse of a matrix.

## Example of use:
## m <- matrix(c(2, 1, 0, 1, 1, 2, 2, 3, 0), nrow = 3, ncol = 3, byrow = TRUE)
## m shoud looks like:
## [,1] [,2] [,3]
## [1,]    2    1    0
## [2,]    1    1    2
## [3,]    2    3    0
## m2 <- makeCacheMatrix(m)
## cacheSolve(m2)
## should return:
## [,1] [,2]   [,3]
## [1,]  0.750  0.0 -0.250
## [2,] -0.500  0.0  0.500
## [3,] -0.125  0.5 -0.125
##
## m3 <- cacheSolve(m2)
## This should display a "getting cached inverse" message
## print(m3)
## should return
## getting cached inverse
## [,1] [,2]   [,3]
## [1,]  0.750  0.0 -0.250
## [2,] -0.500  0.0  0.500
## [3,] -0.125  0.5 -0.125

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(iMatrix) i <<- iMatrix
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
