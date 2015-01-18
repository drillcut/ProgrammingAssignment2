## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - set the value of the inverse
## getinverse - get the value of the inverse
## usage example: 
## > m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
