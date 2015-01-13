## Matrix inversion is usually a costly computation and there is some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly below two function does inverse the matrix and cache it

## This function creates a special "matrix" object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
        i <- NULL
        
        #set the value of the vector
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        
        #get the value of the vector
        get <- function() x
        
        #set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        
        #get the value of the inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the function will retrieve the inverse from the cache
## solve function is used for computing the inverse of a square matrix

cacheSolve <- function(x, ...) {
        
        #check for caching and return inverse matrix if found
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        #compute inverse of matrix
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
