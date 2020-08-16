## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that cache the inverse of a matrix.
## It is assumed that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {  ##Set the matrix 
                  x <<- y
                  inv <<- NULL
                }
                get <- function() x  ##Get the matrix
                setInverse <- function(inverse) inv <<- inverse  ##Setting inverse of the matrix
                getInverse <- function() inv.  ##Getting inverse of the matrix
                list(set = set,
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getInverse()
                if (!is.null(inv)) {  ##If inverse already exists, get the cached value
                  message("getting cached matrix")
                  return(inv)
                }
                mat <- x$get()   
                inv <- solve(mat, ...)  ##If inverse is not present, use solve function to calculate the inverse of the function
                x$setInverse(inv)   ##Set the inverse of the matrix
                inv
}
