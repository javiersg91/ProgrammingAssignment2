## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special matrix which is a list containing a function
##to set and get the values of the matrix and set and get the values of the 
##inverse
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
        set <- function(y){
            x <<- y
            y <<- NULL
        }
            
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
##This function calculates the inverse of the special matrix created before,
##checking if it has been previously calculated, recovering it from the cache 
##instead of recalculating it if available. If not available, it calculates the
##inverse and sets the value in the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
