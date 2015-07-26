"
Example: Caching the Mean of a Vector

In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean.

The first function, makeVector creates a special "vector", which is really a list containing a function to
set the value of the vector
get the value of the vector
set the value of the mean
get the value of the mean
makeVector <- function(x = numeric()) {
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
get <- function() x
setmean <- function(mean) m <<- mean
getmean <- function() m
list(set = set, get = get,
setmean = setmean,
getmean = getmean)
}
The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
m <- x$getmean()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- mean(data, ...)
x$setmean(m)
m
}
"


#  Project Question
"
Write the following functions:
  
  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
"

# Solution 

makeCacheMatrix <- function (x <-  matrix()) {
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# The function solve solves the equation a %*% x = b for x, where b can be either a vector or a matrix.


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
