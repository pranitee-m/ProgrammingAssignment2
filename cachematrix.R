library(MASS)
makeCacheMatrix <- function(x = matrix()) {  ##makeCacheMatrix is made a function
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {
    inver<-ginv(x)   ##gives the inverse
    inver%*%x
  } 
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv
       

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
  inv                     ##return a matrix that is inverse of 'x'
}
