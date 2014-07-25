## Programming Assignment 2: Lexical Scoping
## In this assignment we'll be writing a function to cache the result of the solve function over that matrix
## the 1st time the cacheSolve is called, the solve function is called and the result is returned
## any subsequent call to the cacheSolve, the cached result is returned without any computations


## the makeCacheMatrix has 4 functions:
##    set to assign object a matrix and clear the cache of the solve
##    get to return the source matrix as created
##    setSolveMatrix to solve the matrix
##    getSolveMatrix to return the solved matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL ## Clearing the cached value
  }
  
  get <- function() x
  
  setSolveMatrix <- function(solve) m <<- solve
  
  getSolveMatrix <- function() m
  
  list(set = set, get = get,
       setSolveMatrix = setSolveMatrix,
       getSolveMatrix = getSolveMatrix)
  
}


## cacheSolve function solves a matrix and caches its result, and returns it back
## in case the matrix's solve was cached, it notifies the user that it used the cached result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolveMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolveMatrix(m)
  m
}