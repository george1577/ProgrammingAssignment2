# Create a function makeCacheMatrix, take a matrix as an argument, and it can cache its inverse
## makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function(){
      x
    }
    setinv <- function(inverse){
      inv <<- inverse
    }
    getinv <- function(){
      inv
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}

# Create another function cacheSolve, which computes the inverse of the matrix returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat)
  x$setinv(inv)
  inv
}
