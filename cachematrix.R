## This program will attempt to return the inverse of a square
## matrix by checking to see if the inverse has already been 
## calculated and returning the value

## the makeCacheMatrix function creates the object makeCascheMatrix
## that gets and sets the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##set function
  set <- function(y) {
    x <<- y
   inv <<- NULL
  }
  ## get function
  get <- function() x
  ## defines a setter for the solve function
  setinv <- function(solve) inv <<- solve
  ## defines a getter for the solve function
  getinv <- function() inv
  
  ##the list returned allow the $ to access function by name
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve is used to populate or retieve the inverse of the 
## makeCacheVector object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## check to see if the inverse exists then retrieves it
  if(!is.null(inv)) {
    message("getting cached inverse matrix data")
    return(inv)
  }
  ## If it does not exist, data is assign the value from the get
  data <- x$get()
  ## inverse is calculated
  inv <- solve(data, ...)
  ## the value is now set
  x$setinv(inv)
  ## the inverse returned
  inv
}
