## Caching the Inverse of a Matrix (programming assignment 2)
## cacheMatrix.R  Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly.  The purpose of the two functions below is to cache the 
## inverse of a matrix.

## makeCacheMatrix This function creates a special "matrix" object that can 
## cache its inverse by doing the following:
## 1 - Set the values of the matrix
## 2 - Get the values of the matrix
## 3 - Set the inverse values of the matrix
## 4 - Get the inverse values of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## This function uses the solve() function which returns the inverse of 
## an object.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInverse(m)
        m
}

