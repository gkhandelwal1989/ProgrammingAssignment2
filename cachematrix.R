## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmatrix <- function(tempMatrix) mat <<- tempMatrix
  getmatrix <- function() mat
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
    mat <- x$getmatrix()
    if(!is.null(mat)) {
      message("getting cached data")
      return(mat)
    }
    data <- x$get()
    mat <- solve(data)
    x$setmatrix(mat)
    mat
  }
