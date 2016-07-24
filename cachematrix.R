## The following functions comprises of two parts. First part creates a special 
## "matrix" object that can cache its inverse and the other part computes the 
## inverse but retreives the inverse from cache if already calculated.


## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                   s <- NULL
                  set <- function(y) {
                       x <<- y
                       s <<- NULL
}
get <- function() x
setinv_matrix <- function(inv_matrix) s <<- inv_matrix
getinv_matrix <- function() s
list(set = set, get = get,
     setinv_matrix = setinv_matrix,
     getinv_matrix = getinv_matrix)
}

##  This function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then cacheSolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
s <- x$getinv_matrix()
           if(!is.null(s)) {
                   message("getting cached data")
                    return(s)
                }  
                data <- x$get()
                s <- solve(data, ...)
                x$setinv_matrix(s)
                s

        ## Return a matrix that is the inverse of 'x'
}
