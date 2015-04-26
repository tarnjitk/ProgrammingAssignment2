## The functions makeCacheMatrix and cacheSolve work in conjunction when solving the inverse of
## square matrices. Large matrix inversions can be tackled by relying on memonized solutions of 
## previously calculated matrix inversion. The solve() function is used for the matrix inversion, 
## alternatively the ginv() function could be employed.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Here x is a square matrix, defined by matrix(data, nrow, ncol, byrow)

makeCacheMatrix <- function(x = matrix()) {
    
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(solve) m <<- inverse
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)

}


  ## cacheSolve: This function computes the inverse of the special "matrix" returned by 
  ## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrixInverse()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}

