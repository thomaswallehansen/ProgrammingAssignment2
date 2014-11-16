## These two functions are used to create a matrix object and
## then calculate the inverse or get it from the cache if it
## has already been calculated. makeCacheMatrix creates a
## matrix object that can cache it's inverse. caceheSolve 
## computes the inverse of the special "matrix" returned 
## by makeCacheMatrix if the inverse hasnt been saved in 
## the cache


## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  #Initialise pointer i to NULL
  i <- NULL
  
  #Set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Get the matrix
  get <- function() x
  
  #Set and get the inverse
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  #Get pointer to inverse
  i <- x$getInverse()
  
  #If the pointer is not NULL, get cahced inverse and return
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #Else, (the pointer is NULL) get the data and calculate inverse
  data <- x$get()
  i <- solve(data, ...)
  
  #Save pointer to inverse
  x$setInverse(i)
  
  #Return inverse
  i  
}
