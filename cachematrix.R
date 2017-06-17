## Here, I wrote two functions in order to cache the inverse of a matrix
## for faster calculations. This is my assignment for week 3

## The makeCacheMatrix function creates a list, which has the initial matrix
## and the inverted matrix saved. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## The cachSolve function checks if the inverse of the matrix from makeCacheMatrix 
## was already calculated and saved. If not, it is calculated and saved in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
