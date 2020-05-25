## Below are two functions that can be used to cache the inverse of a matrix
## which will save us time from having to recompute the inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # set x default value to empty matrix
  m <- NULL # set m to null and initialize as an object within makeCacheMatrix env
  set <- function(y) {
    x <<- y #Assign input value to x object in the parent environment
    m <<- NULL #Assign value of null to the m object in parent env; clears possible prev val
  }
  get <- function() x # retrieves x from parent env of makeCacheMatrix
  setinv <- function(solve) m <<- solve
  #finds the inverse and assigns that value to m in the parent env
  getinv <- function() m #retrieves m from parent env
  list(set = set, # gives name 'set' to the set() function
       get = get, # gives name 'get' to get() function
       setinv = setinv, # gives name 'setinv' to setinv() function
       getinv = getinv) # gives name 'getinv' to getinv() function
  #naming list elements allows us to use $ in cacheSolve to extract contents
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() # calls getinv() to retrieve inv
  if(!is.null(m)) { #check to see if we have a cached inv
    message("getting cached data")
    return(m) # returns cached inverse value of matrix
  }
  data <- x$get() #retrieves our matrix
  m <- solve(data, ...) #solves for the inverse of the matrix
  x$setinv(m) #sets in the inverse of the matrix
  m # returns the inverse of the matrix
}
