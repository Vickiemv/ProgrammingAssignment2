##The makeCacheMatrix is similar to 'makeVector' function given in the example. It creates a 
##special matrix object which returns list of functions that will be available to the user in
##the global environment.

# makeCacheMatrix has four functions returned to the parent environment. This sets the value of 
# matrix and also sets its inverse.

makeCacheMatrix <- function(x = matrix()) {
     I<-NULL
     set <- function(y) {
          x <<- y
          I <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) I <<- inverse
     getinverse <- function() I
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

# cacheSolve returns the inverse of the matrix that is already in the cache if not it finds the
# inverse of the matrix and returns that value


cacheSolve <- function(x, ...) {
     I <- x$getinverse()
     if(!is.null(I)) {
          message("getting cached data")
          return(I)
     }
     data <- x$get()
     I <- solve(data, ...)
     x$setinverse(I)
     I
}


##---------EXAMPLE----------##


# a=matrix(c(2,5,1,4),2,2)
# b <- makeCacheMatrix(a)
# cacheSolve(b)

