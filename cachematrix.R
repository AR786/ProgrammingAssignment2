## The first function makeCacheMatrix accepts a matrix and converts it into a special
## matrix with following functions:
##    get:    returns the special matrix
##    set:    set the matrix values
##    getinv: returns the inverse if calculated or else NULL value
##    setinv: sets the inverse and caches it for the next time


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getinv <- function() m
  setinv <- function(inv) m <<- inv
  list(set = set,  get = get,  getinv = getinv,  setinv = setinv)
}


## This function gets the cache inverse value if calculated earlier else
## it will calculate it and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
