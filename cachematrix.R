

## In makeCacheMatrix function it creates a matrix to cache the inverse of a matrix
## We will inverse the matrix in the cahceSolve function

makeCacheMatrix <- function(x = matrix()) {
  
  
  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## the following function find the inverse of matrix x using the inbuilt R solve function
cacheSolve <- function(x, ...) {
 
  inv = x$getinv()
  

  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  

  mat.data = x$get()
  inv = solve(mat.data, ...)

  x$setinv(inv)
  
  return(inv)
}
