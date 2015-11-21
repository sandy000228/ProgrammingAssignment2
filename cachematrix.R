## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function (y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse_matrix <<- inverse
        getInverse <- function() inverse_matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}


## This function computes the inverse of the special "matrix" created by 
## makesCasheMatrix above. It first checks if the inverse matrix has already been calculated. 
## If so get the inverse matrix and skips the computation.
## Otherwise, it calculate the inverse matrix and set the value of inverse matrix in the cache via the setInverse function.


cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getInverse()
        if (!is.null(inverse_matrix)){
                message("getting cached data")
                return(inverse_matrix)
        }
        mat <- x$get()
        inverse_matrix <- solve(mat)
        x$setInverse(inverse_matrix)
        inverse_matrix
}
