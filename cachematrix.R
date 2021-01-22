## Put comments here that give an overall description of what your
## functions do

## Creates a list objects with elements that caches the matrix inverse
## Elements of the list:
## set(matrix) - function to set the matrix
## get - function to get the matrix
## setinverse(inverse) - function to cache the matrix inverse
## getinverse - funcion to get the cached matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  x.inverse <- NULL
  set <- function(new_x)
  {
    x <<- new_x
    x.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x.inverse <<- inverse
  getinverse <- function() x.inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the function list returned by makeCacheMatrix.
## If the inverse has already been calculated It shows the message "getting cached inverse matrix" and retrieve the cached matrix inverse
## It the inverse has not been calculeted, it calculates the inverse (using solve function) and update the cache info.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x.inverse <- x$getinverse()
  if(!is.null(x.inverse)){
    message("getting cached inverse matrix")
    return(x.inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}


#to teste
#a <- matrix(c(2, 1, 5, 3), nrow = 2, ncol = 2)
#cache = makeCacheMatrix(a)
#cacheSolve(cache)