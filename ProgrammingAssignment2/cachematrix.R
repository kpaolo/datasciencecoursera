## These pair of R functions(makeCacheMatrix and cacheSolve returns the inverse of a matrix and caches it.

  
##makeCacheMatrix creates and returns a list of functions
##used by cacheSolve to get or set the interted matrix in chache

makeCacheMatrix <- function(x = matrix()){
  #stores the cached value
  #initialize to NULL
  cache <- NULL
  
  #create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  #gets the value of the matrix
  get <- function() x
  
  #invert the matrix and store in cache
  setMatrixInverse <- function(solve) cache <<- solve
  
  #get the inverted matrix from cache
  getMatrixInverse <- function() cache
  
  #returns the created functions to the working environment
  list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}


##cacheSolve calculates the inverse of the matrix created by makeCacheMatrix
##If the inverse has already been calculated(and the matrix has not changed), 
##then it should retrieve the inverse

cacheSolve <- function(x, ...) {
  ##attempt to get the inverse of the matrix stored in cache
  cache <- x$getMatrixInverse()
  
  #return inverted matrix from cache if it exists
  #else create the matrix in working environment
  if(!is.null(cache)){
    message("getting cached data")
    
    #display matrix in console
    return(cache)
  }
  
  #create matrix since it doesn't exisst
  matrixData <- x$get()
  
  #set and return inverse of matrix
  cache <- solve(matrixData, ...)
  
  #set inverted matrix in cache
  x$setMatrixInverse(cache)
  
  #display matrix in console
  return(cache)
}
  
  #create matrix since it doesn't exisst
  matrixData <- x$get()
  
  #set and return inverse of matrix
  cache <- solve(matrixData, ...)
  
  #set inverted matrix in cache
  x$setMatrixInverse(cache)
  
  #display matrix in console
  return(cache)
}