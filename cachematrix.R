# This R script is part of Assignment 2 of R Programming on Coursera offered by John Hopkins. 

##  makeCacheMatrix is a function that will create a special matrix. It will return a list of functions:
##  * set()
##  * get()
##  * setmatrix()
##  * getmatrix()
# passed argument to this function is a matrix..
makeCacheMatrix <- function(x = matrix()) {
  # initially mat will get set to NULL
  mat <- NULL # mat is actually the variable which will store inverse of matrix
  # this will set the value of the matrix passed as parameter i.e y
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  #this will return original matrix 
  get <- function() x
  # this function will set matrix to tempmatrix which is inverse of a matrix
  setmatrix <- function(tempMatrix) mat <<- tempMatrix
  #this will return matrix which is the resultant inverse matrix
  getmatrix <- function() mat
  # a list of four functions is returned i.e set,get,setmatrix and getmatrix..
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## CacheSolve is a function that will return inverse of a matrix either from cache or
# or will compute and in this case save to cache and will return computed matrix
#special matrix.. i.e list returned by makeCacheMatrix will be used an argument to this function
# and it will return inverse of a matrix.
cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
    # this is calling getmatrix which will return null or inverse of matrix.
    # if it is for first time
    # null is returned else it will return inverse of matrix.
    mat <- x$getmatrix()
    if(!is.null(mat)) {
      # if there is already cached inverse matrix
      message("getting cached data")
      #inverse of a matrix is returned
      return(mat)
    }
    #getmatrix returned null.
    
    ## here data will get value of original matrix.
    data <- x$get()
    # solve is used to do the inverse of a matrix
    mat <- solve(data)
    # setmatrix is done which will be used to cached inverse of a matrix at later stage ..
    # it is used to save inverse of original matrix
    x$setmatrix(mat)
    #inverse of a matrix is returned...
    mat
  }
