## This function creates a special "matrix", a list containing a
## function to: 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setinverse<- function(inverse) i<<- inverse
  getinverse<- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## This function calculates the inverse of the special 
## "matrix" created above. First, it checks to see if 
## the inverse already exists. If so, it gets the 
## inverse from the cache and it skips calculation. 
## Otherwise, it calculates the mean of the data and set
## the inverse in cache by means of the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
