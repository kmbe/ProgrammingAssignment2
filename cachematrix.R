## makeCacheMatrix() creates an R object that stores a matrix and the matrix's inverse
## x is initalised as a function argument, inialising it as an object for makeCacheMatrix()
## Inv is where the value for the inverse for matrix x will be stored
## set takes an argument, y, and assumes the value is a matrix
## clearing any previous values for Inv that were cached in a prior execution.


makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL
      set <- function(y) {
            x <<- y
            Inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) Inv <<- solve
      getinverse <- function() Inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Requires an argument that is returned by makeCacheMatrix() in order to retreive the inverse
## of the matrix from the cached value that is stored in makeCacheMatrix() object's enivornemnt.

cacheSolve <- function(x, ...) {
       Inv <- x$getinverse()
       if(!is.null(Inv)) {      
          return(Inv)
       }
       data <- x$get()
       Inv <- solve(data, ...)
       x$setinverse(Inv)
       Inv
  ## Return a matrix that is the inverse of 'x'
}

