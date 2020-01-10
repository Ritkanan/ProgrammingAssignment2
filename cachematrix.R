## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  
  i <- NULL
  
  ## set the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  
  ## set the matrix
  get <- function() {
    m
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ##get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above
## If the inverse has already been calculated (and the matrix has not changed,
## then the "cachesolve" should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  ## return inverse 'x'
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  m
}
