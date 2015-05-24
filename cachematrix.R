## Example
# > m=rbind(c(3, 5), c(2, 4))
# > cm <- makeCacheMatrix(m)
# > cacheSolve(cm)
#      [,1] [,2]
# [1,]    2 -2.5
# [2,]   -1  1.5
# > cacheSolve(cm)
# getting cached data
#      [,1] [,2]
# [1,]    2 -2.5
# [2,]   -1  1.5

## This function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# get/set the value of the matrix
# get/set the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This will solve inverse matrix of an object created with makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Get matrix from the special "matrix" created with makeCacheMatrix
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}