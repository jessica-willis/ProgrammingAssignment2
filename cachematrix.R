## the goal of this project is to create a method that will take a 
## matrix and invert it and use a cache for retrieving the inverted matrix

## creates a special matrix object that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## computes the inverse or retrieves it from the cache
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
