## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  # defining the inverse of the matrix to null
  set <- function(y) { # function to set the value of matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x #function to get the matrix
  
  setInverse <- function(inv) inverse <<- inv  # function to set the inverse of the matrix
  getInverse <- function() inverse # function to get the inverse of the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #returning  a list with all functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse() # get the inverse of the matrix
  if(!is.null(inverse)) { # if the inverse of the matrix is  defined
    message("getting cached data")
    return(inverse) # just get the value in the cache
  }
  data <- x$get() # if not defined, get the matrix
  inverse <- solve(data, ...) # calculate the inverse
  x$setInverse(inverse) # save the inverse in the cache
  inverse # return the inverse
}