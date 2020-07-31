
##makeCacheMatrix and cacheSolve calculate and cache the inverse of the matrix inputted to makeCacheMatrix.

## makeCacheMatrix sets the matrix and the functions needed to supply information to cacheSolve
#

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL  #clears any value of m from previous execution of cacheSolve
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set =set, get = get,        #names the elements so can use $extractor in cacheSolve
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix from makeCacheMatrix and if unchanged returns the cached data.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m       
}
