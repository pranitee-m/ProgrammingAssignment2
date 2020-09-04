library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {
    inver<-ginv(x)
    inver%*%x
  } 
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

##Please include your own comment to explain your code (Required in Rubric)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data() <- x$get()
  inv <- solve(data(),...)
  x$setinv(inv)
  inv  ##return a matrix that is inverse of 'x'
}
