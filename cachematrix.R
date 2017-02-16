## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## This is a pair of functions that cache the inverse of a matrix.


## The makeCacheMatrix function creates a list of functions that set the matrix,
## get the matrix, set the inverse matrix, and get the inversematrix

makeCacheMatrix <- function(x=matrix()){
     i = NULL
     set <- function(y){
          y <<- x
          i <<- NULL
     }
     get <- function() x
     setinversematrix <- function(solve) i <<- solve
     getinversematrix <- function() i
     list(set = set, get = set, 
          setinversematrix=setinversematrix, 
          getinversematrix=getinversematrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinversematrix()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinversematrix(i)
     i
}