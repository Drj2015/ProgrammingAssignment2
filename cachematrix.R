# Matrix inversion is highly commputation demanding and resouces can be optimized
# by caching the inverse of a matrix rather than compute it repeatedly 
#if matrix elements are not changine. 
#Following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns matrix inverse. It first checks if
# the inverse has already been computed. If it exits, it gets the result 
# without the computation. If not, it computes the inverse, sets the value 
# in the cache via setinverse function.

# It is assumed that the input matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test vectors:
## > x = rbind(c(1, 2), c(2, 1))
## > m = makeCacheMatrix(x)
## > m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    2
##[2,]    2    1
##> 


## No cache in the first run
## > cacheSolve(m)
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##> 


## Retrieving from the cache in the second run
## > cacheSolve(m)
##getting cached data.
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##> 