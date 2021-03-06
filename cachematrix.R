## This function creates inverse of matrix and returns list

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of a matrix if it is not already created
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
