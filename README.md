 ## makeCacheMatrix() - This function creates a special "matrix" object that can cache its inverse.
## cacheSolve() - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
makeCatcheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y=matrix()){
    x <<- y
    inv <<- NULL
  }
  get <- function (){
    x
  }
  setinv <- function(i){
    inv <<- i
  }
  getinv <- function(){
    inv
  }
  list(set = set,get = get,setinv = setinv, getinv = getinv)
}
## The following function calculates the inverse of the "special" matrix, created by the 'makeCacheMatrix' funtion.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
