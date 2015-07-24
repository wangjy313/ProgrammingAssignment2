## cachematrix is to write a pare of functions that cache the inverse of a matrix. 

## makeCachematrix defines a list of functions, including:
# 1. set: 1) set the value of the matrix
#         2) clean the inverse matrix of the old one from cache 
# 2. get: get the value of the matrix
# 3. setinverse: set the inversed matrix in cache
# 4. getinverse: get the inversed matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  
  # initial the inversed matrix object to be NULL
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## CacheSolve returns the inverse matrix:
# 1. checks whether the inverse matrix has been computed and saved in cache.
# 2. if yes, it returns the inverse matrix from cache.
# 3. otherwise, 1) compute the inverse matrix via 'solve' function
#               2) save the result in cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##Example to test the code
#> m <- matrix(data=1:4, nrow=2)
#> m1 <- makeCacheMatrix(m)
#> cacheSolve(m1)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(m1)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

