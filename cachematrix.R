#makeCacheMatrix creates a special "matrix" and cacheSolve calculates the inverse of the special "matrix".
#If the inverse is already calculated, it gets the inverse from the cache. Otherwise, it will compute the inverse.

#makeCacheMatrix creates a special "matrix" object which is a list containing functions to do the following:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#cacheSolve calculates the inverse of the special "matrix" created from makeCacheMatrix. It first checks if 
#the inverse is calculated. If it is and the matrix has not changed, it grabs the inverse from the cache and returns it. 
#Otherwise, it calculates the inverse of the matrix, sets the value of the inverse in the cache through the
#setinverse function, and returns the computed inverse.

cacheSolve <- function(x, ...) {
         i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
