## Sorry for my English
## My function makes a matrix with the row and column you want and it's intervals
## i set the variables and the function 'z'

## This function makes a matrix with certain lines, columns and intervals that can be stored in a variable.
makeCacheMatrix <- function(x = matrix()) {
  inverso <- NULL
  set <- function(z) {
    x <<- z
    inverso <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverso <<- inverse
  getInverse <- function() inverso
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## this function get the inverse of 'x'
## I give the inverso variable (inverso means inverse in my native language) the inverse value of 'x'
## And i solve the matrix

cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverso <- x$getInverse()
  if(!is.null(inverso)) {
    message("Resolvendo")
    return(inverso)
  }
  mat <- x$get()
  inverso <- solve(mat, ...)
  x$setInverse(inverso)
  inverso
}
