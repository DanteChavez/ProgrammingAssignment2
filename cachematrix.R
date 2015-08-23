# Functions to compute the inverse of a matrix with caching.

# Caching function for a matrix argument.
# Returns a vector of get and set for the matrix itself
# for the matrix inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Obtains the inverse of an assumed square matrix,
# or retrieves a previously calculated inverse from cache, given a list
# from the function makeCacheMatrix().

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m
}