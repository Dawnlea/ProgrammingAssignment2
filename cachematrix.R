## The following function will create a special matrix object 
## that can cache its inverscache the inverse of a supplied matrix

makeCacheMatrix <- function(baseMatrix = matrix()) {
  ## Initialise matrix
  invM <- NULL 
  set <- function(y) {
    baseMatrix <<- y
    invMatrix <<- NULL
  }
  ## return the uninverted matrix
  get <- function() baseMatrix
  ## set the cached matrix to the inverse matrix supplied
  setMatrix <- function(inMatrix) invMatrix <<- inMatrix
  ## retrieve the inverted matrix  
  getMatrix <- function() invMatrix
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## The following function computes the inverse of the special matrix
## returned by makeCacheMatrix
## When the inverse has already been calculated, the inverse is 
## retrieved from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getMatrix()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  baseMatrix <- x$get()
  invMatrix <- solve(baseMatrix, ...)
  x$setMatrix(invMatrix)
  print(invMatrix)
}
