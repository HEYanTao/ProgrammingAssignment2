## These two functions work together to calculate the inverse matrix of a given matrix.
##Since the calculation of inverse matrix can be quite complicated, it's better to cache those
##results of already calculated matrixes.

## makeCacheMatrix is a function generating several functions.
##It can be viewd as generating a class in other OOP languages such as C# or Visual Basic.
##This class contains the data and operations to this special datatype "makeCacheMatrix".

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL ##inverse stores the inversed matrix
      set <- function(y) {
            x <<- y ##x stores the initial matrix
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve calculate the inverse matrix. If this matrix has been calculated before,
##function will read cache instead of solving it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      ##first find out whether this matrix has been calculated or not
      if(!is.null(m)) {
            message("getting cached data")
            ##if has calculated then read the cached data
            return(m)
      }
      data <- x$get()
      ##if has not calculated, then get data to calculated
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
##An example of how to use
##step1: set wd to corresponding folder
##step2: source("cachematrix.R")
##step3: create a matrix named data
##step4: x<-makeCacheMatrix(data)
##step5: cacheSolve(x)
##you shall see the inversed matrix of your input
##step6: x<-makeCacheMatrix(data)
##you shall see the following:"getting cached data" and your result
