# homework2

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) xinv <<- inverse
  getInverse <- function() xinv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  invFunc <- x$getInverse()  ## getting the previous defined matrix
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)  ##obtaining the matrix
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
 
##testing the function
matrix=matrix(c(2,1,-1,-1),nrow=2)
testing=makeCacheMatrix(matrix)
cacheSolve(testing)
