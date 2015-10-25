## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  mymatrix<- NULL 
  setmatrix <- function(y)
  {
    mymatrix <<- y 
    inv <<- NULL 
  }
  getMatrix <- function(){mymatrix}
  
  ##Inverse Calculation 
  getInv <- function (){ inv}
  setInv  <- function (val) { inv <<- val}
  
  list (setmatrix = setmatrix, getMatrix = getMatrix, getinv = getinv, setinv = setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv))
  {
    ## inverse has been calculated, let us check if the matrixes are same 
    if(identical(mymatrix, x$getMatrix()))
    {
      message("getting cached data")
      return (inv)
    }
    
    mymatrix <- x$getMatrix()
    x$setmatrix(mymatrix)
    inv <- solve(mymatrix,...)
    x$setInverse(inv)
    return(inv)
  }
}
