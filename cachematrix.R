## makeCacheMatrix
##  returns a list for performing the following functions
##   - setting the matrix (set)
##   - getting the matrix (get)
##   - setting the inverse of the matrix (setinverse)
##   - getting the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  inv<- NULL                             # initialize the inverse of the matrix
  set <- function(y) {
    x <<- y                              # the <<- is a super assignment operator
    inv <<- NULL                         # to update the variable in the parent environment
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(solve) inv <<- solve  
  
  ## get the inverse of the matrix
  getinverse <- function() inv
  
  ## return the requested list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve
##  returns a matrix depending on the following:
##  - if the cache exists, return its contents
##  - if the cache does not exist, compute the inverse of the matrix
##    and store the results in the matrix

cacheSolve <- function(x, ...) {
  
  ## retrieve the inverse for the matrix        
  inv <- x$getinverse()
  
  ## check the contents of the matrix 
  if(!is.null(inv)) {                        # the matrix found, return it from the cache
    message("getting cached data")           # inform the user that the data is from the cache
    return(inv)
  }
  ## if the matrix is empty, return the inverse   
  data <- x$get()
  inv <- solve(data, ...)                    # solve is a generic function that solves an equation
  x$setinverse(inv)
  inv
}


