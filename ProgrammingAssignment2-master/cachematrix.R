## This script caches the inverse of a matrix

## creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #initalize m to null - haven't taken inverse
  m <- NULL 
  #calling get returns the original matrix
  get <- function() x
  #set the matrix with tis function
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #computes the inverse of x and assigns the vlaue to m
  setinverse <- function() m <<- solve(x)
  #returns the inverse
  getinverse <- function() m
  #outputs a list with all possible desired values 
  list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
}




#This function computes the inverse of the matrix
#it call upon the makeCacheMatrix function 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #see if inverse has been computed using the marker- m
  #if have already taken inverse (ie m isnt null), output m
  m <- x$getinverse()
  if(!is.null(m)){
    return(m)
  }
  #code executes here if the inverse of matrix hasn't been computed 
  #computes and returns the inverse
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}


