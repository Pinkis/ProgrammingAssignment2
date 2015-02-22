## Thank you for reviewing my programming, English is not my mother language, so
## please forgive mistakes.

## These functions are to help make matrix inverting faster. 
## The first function makes a matrix that has place to store inverse of itself. 
## The second function will return the inverse of input matrix. If it has been
## used before, it will use the stored value instead of calculating again. 


## This function makes list from input matrix argument. In this list will be found
## the matrix from input, and later will be stored the inverse of this matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {## Assign matrix value to y
    x <<- y
    m <<- NULL ## Clear stored inverse matrix
  }
  get <- function() {x} # Give matrix that was input back out
  setsolve <- function(solved) {m <<- solved} ## Assign inverse matrix to solved
  getsolve <- function() {m} ## Give inverted matrix if is stored
  matrixList <- list(set = set, get = get, ## Prepare list that will be returned by function
       setsolve = setsolve,
       getsolve = getsolve)
  
  return(matrixList) ## explicit return list (more clear than implicit list()...)
}


## Here is function for getting inverse of matrix. Only works for matrix things
## made by makeCacheMatrix. First, checks if already solved, if yes, give stored
## value. If no, solve matrix, give value, and store value in matrix list.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) { ## checks x$getsolve(), for existing solution
    message("cache exists, using cache data")
    return(m) ## give stored value
  }
  ## If no existing solution
  data <- x$get() ## get actual matrix data from list
  m <- solve(data, ...) ## solve matrix for inverse
  x$setsolve(m) ## store inverse in list
  return(m) ## explicit return of inverse solution 
}