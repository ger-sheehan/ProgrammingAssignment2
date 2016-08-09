## makeCacheMatrix 
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks of the inverse has already been calculated. If yes,
## returns the cached verion, otherwise calculated the 
## inverse with solve()

cacheSolve <- function(x, ...) {
  #get current vlaue
  inv <- x$getinv()
  
  #If I've already calculated the inverse
  if(!is.null(inv)) {
    #done. Return value
    return(inv)
  }
  #calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  #set value
  x$setinv(inv)
  
  #return the inverse
  inv
}
