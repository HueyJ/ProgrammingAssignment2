## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL

  # set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  # get the value of the matrix
  get <- function() x

  # set the value of the inverse
  setinverse <- function(inverse) i <<- inverse

  # get the value of the inverse
  getinverse <- function() i

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {

  i <- x$getinverse()

  # If the inverse has already been calculated
  if(!is.null(i)) {
    message("getting cached data")
    # then the cachesolve should retrieve the inverse from the cache.
    return(i)
  }

  # computes the inverse of the special "matrix"
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)

  i
}
