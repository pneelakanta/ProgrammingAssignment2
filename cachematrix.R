## makeCacheMatrix function creates a specia; vector which provides the functions to get/set the value of vector and get/set value of mean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL #Reinitialize the new matrix to Null
  }
  get <- function() x
  ##Set Inverse
  setinverse <- function(inverse) m <<- solve
  ##Get Inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of the special "vector" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  # checks if inv is null. if not return cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() ##If it is not null
  m <- solve(data, ...)
  x$setinverse(m) ##cache the data
  m
}
