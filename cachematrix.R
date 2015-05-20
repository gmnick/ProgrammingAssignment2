## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix: Creates a "super" matrix which provides four functions to:
##                           1. Set the value of the matrix           --- set
##                           2. Get the value of the stored matrix    --- get
##                           3. Set the value of the matrix inverse   --- setinv
##                           4. Get the value of the matrix inverse   --- getinv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## function cacheSolve: this function calculates the inverse of the "matrix" created by the function "makeCacheMatrix".
##                      If the inverse was already computed, it returns the stored value.
##                      Otherwise, it calculates the inverse of the data and stores the value of the 
##                      inverse in the cache using the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
