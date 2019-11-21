## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # Define i object that will store the inverse matrix
  i <- NULL
  
  # set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get matrix
  get <- function() 
      x
  
  # set inverse matrix
  setinverse <- function(inverse) 
    i <<- inverse
  
  #get inverse matrix
  getinverse <- function() 
    i
  
  #return all 4 methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i)) {
    # if inverse matrix is cached already, notify and return it
    message("getting cached data")
    return(i)
  }
  
  # inverse matrix is not yet calculated so calculate and store in the variable.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  i
  
}

