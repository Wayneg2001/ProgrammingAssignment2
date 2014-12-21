## Wayne Greer
## Matrix inversion function
## makeCacheMatrix: creates a special "matrix" object that can cach
## its inverse.
## This functio computes the inverse of the special "matric" retuned by makeCacheMatirx
##
##
## makeCacheMatrix creates a list containing a function to
## 1. Set the valuse of the Matrix
## 2. Get the value of the Matirx
## 3. Set the value of the inverse of the matrix
## 4. Get the v alue of inverse of the matrix
##
##
makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  } 
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

## Wayne Greer
## cachesolve function
### This function assumes that the matirs is always invertible
##
##
cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data.")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setinverse(invs)
  invs
}

  
