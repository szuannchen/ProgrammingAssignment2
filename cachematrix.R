
## These two functions can be used to cached the inverse of the 
## matrix in order to avoid repetitive computation.

##  makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve assumes that the matix supplied is always invertible.
## It first checks if the inverse of the matrix has been computed.
## If so, it retrieve the inverse from the cache without additional 
## computation. If not, it computes and sets the inverse in the 
## cache.
 

cacheSolve <- function(x, ...) {
  I <- x$getinv()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data,...)
  x$setinv(I)
  I  
}