## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The first function, makeVector creates a special "matrix" object that can cache its inverse, 
#which is really a list containing a function to,

# 1)set the value of the matrix
# 2)get the value of the matrix
# 3)set the value of the inverse
# 4)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {    # input a matrix
  inv_mat <- NULL                             # initialize inv_mat as NULL 
  set <- function(y) {                    # set the value of the matrix
    x <<- y                             
    inv_mat <<- NULL                        
  }
  
  get <- function() x                                    # get the value of the matrix
  
  setinverse <- function(inverse) inv_mat <<- inverse  # set the inverse of the matrix 
  getinverse <- function() inv_mat                     # get the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinverse()
  if(!is.null(inv_mat)) {
    message("Getting cached data")
    return(inv_mat)
  }
  m.data <- x$get()
  inv_mat <- solve(m.data, ...)
  x$setinverse(inv_mat)
  inv_mat
}

#TESTING
#x=matrix(c(1:4),2,2)
#Matrix <- makeCacheMatrix(x)
#Matrix$get()
#Matrix$getinverse()

#cacheSolve(Matrix)
#cacheSolve(Matrix)
