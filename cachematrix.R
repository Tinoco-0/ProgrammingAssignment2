## These two functions are use together to calculate the inverse of a matrix and store it for later use. 

## Creates and returns a list that contains the original matrix and the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }

  get <- function() (x)
  setInverse <- function(inverse) (inv <<- inverse)
  getInverse <- function() (inv)
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## A function that computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")

    return(inv)
  }

  ## if the inverse is not in the cache, compute the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv      
}
