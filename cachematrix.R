##chacematerix.R

## This function creates a special "matrix" object that can cache its inverse
## inv is set to NULL 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##
##makeCacheMatrix is listed above. 
##
#If the inverse has already been computed (and the matrix has not changed), 
#cachesolve can retrieve it from the cache.
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


## Test the functions with a random square matrix

test1 <- matrix(rnorm(50), 8, 8)
test2 <- makeCacheMatrix(test1)
cacheSolve(test2)