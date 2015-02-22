## makeCacheMatrix is a function to handle the caching of inversing a matrix
## cacheSolve does the actually inversing of the matrix

## helper function cache the results of matrix inversion
makeCacheMatrix <- function(init_matrix = matrix()) {
  m <- NULL
  set <- function(y) {
    init_matrix <<- y
    m <<- NULL
  }
  get <- function() init_matrix
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## return a matrix which is an inverse of the matrix passed in as
## an argumnet
cacheSolve <- function(init_matrix, ...) {
  inversed_matrix <- init_matrix$getinverse()
  if(!is.null(inversed_matrix)) {
    message("getting cached data")
    return(inversed_matrix)
  }
  data <- init_matrix$get()
  inversed_matrix <- solve(data, ...)
  init_matrix$setinverse(inversed_matrix)
  inversed_matrix
}
