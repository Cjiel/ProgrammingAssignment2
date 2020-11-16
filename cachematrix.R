## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, `makematrix` creates a special "matrix", which is
##really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix
## Create cachematric from matrix, before using cache solve


makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      x <- data.frame()
      s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }

## Write a short comment describing this function
## Solves cachematrix created with function "makeCacheMatrix".
## If cachesolve was already run on cached matrix, returns previous solve.
## returns an inverse of cachematrix

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
  }    