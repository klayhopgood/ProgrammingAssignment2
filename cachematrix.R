## the aim in the assignment is to write two functions, called "makeCacheMatrix"
## and "cacheSolve" that cache the inverse of a matrix.

## "makeCacheMatrix" is a function that will create a "matrix" that can cache
## its inverse for the input, which is an invertible square matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## "cacheSolve" is a function which solves the inverse of the "matrix" which was
## returned by makeCacheMatrix function above. The inverse has already been  
## calculated, and the cachesolve function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
