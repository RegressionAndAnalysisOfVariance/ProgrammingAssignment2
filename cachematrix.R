## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# create a special matrix that can also save its inverse beside (of course) itself

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #initialising the variable in which in inverse will be
  
  set <- function(y) {
    x <<- y # the set variable becomes the matrix itself
    m <<- NULL # new variable, no inverse yet known
  }
  
  get <- function() x # return x
  
  setinverse <- function(inverse) m <<- inverse # allocate inverse to m
  getinverse <- function() m # return m (inverse)
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# Return the inverse of the matrix x, if it is already know just give it, 
# otherwise, calculate it and save it for next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() # trying to get the inverse of x
  if(!is.null(m)) { # if inverse is already known, return that 
    return(m)
  }
  
  data <- x$get() # if not know, get matrix, calculate inverse and set it to save, afterwich return it
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
