# Create a function called makeCacheMatrix that initializes a matrix 'x' as NULL

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached inverse as NULL
  j <- NULL
  # Defining a 'set' function to set the matrix 'x'
  set <- function(y){
    # Use the <<- operator to assign 'y' to the 'x' in the outer scope
    x <<- y
    # Reset the cached inverse to NULL
    j <<- NULL
  }
  
  # Defining a 'get' function to retrieve the matrix 'x'
  get <- function()x
  
  
  # Defining a 'setInverse' function to set the cached inverse
  setInverse <- function(inverse) j <<- inverse
  
  # Defining a 'getInverse' function to retrieve the cached inverse
  getInverse <- function() j 
  
  # Return a list of the defined functions
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


# Defining a function called cacheSolve that takes a cached matrix object 'x' and calculates its inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  # Check if the inverse is already cached    
  if(!is.null(j)){
    message("getting cached data")
    # If cached, return the cached inverse
    return(j)
  }
  # If the inverse is not cached, retrieve the matrix 'x'
  mat <- x$get()
  
  # Calculate the inverse using the 'solve' function
  j <- solve(mat,...)
  
  # Cache the computed inverse by calling 'setInverse'
  x$setInverse(j)
  j
}