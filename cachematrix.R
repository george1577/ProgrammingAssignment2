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

cacheinv <- function(x, ...){
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
