## makeCacheMatrix

## This function makes use of the <<- operator to cache matrix data
## mtrx will be the cached matrix
## while mtrxinv will contain a cached inverse of mtrx

makeCacheMatrix <- function(mtrx = matrix()) {
  mtrxinv <<- NULL
  
  ## Set the global environment matrix mtrx
  ## Clear any values in the mtrxinv because the original has changed 
  setMatrix <- function(m) {
    mtrx <<- m
    mtrxinv <<- NULL
  }
  
  ## Retreive the Cached matrix 
  getMatrix <- function() {
    mtrx
  } 
  
  ## Otherwise use solve to generate  an inverse matrix
  setInverseMatrix <- function (m) {
    mtrxinv <<- solve(m)

  }
  
  ## Retreive the cached inverse matrix 
  getInverseMatrix <- function () {
    mtrxinv
  }
  
  list (setMatrix = setMatrix, getMatrix = getMatrix, 
        setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
  
}

## cacheSolve 
## Write a short comment describing this function

cacheSolve <- function(m = matrix()) {
  ## Return a matrix that is the inverse of 'x'
  mi <- m$getInverseMatrix()
  
  ## If 
  if (!is.null(mi)) {
    message("Getting cached data ....")
            return(mi)
  }
  
  data <- m$getMatrix()
  
  m$setInverseMatrix(data)
  
  mi <- m$getInverseMatrix()
 
  mi
}
