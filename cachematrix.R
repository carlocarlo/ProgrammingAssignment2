## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a list containing functions of getting and setting matrix and inversed one.

makeCacheMatrix <- function(x = matrix()) {
  
  #inversed_matrix is a variable that stores the inversed matrix
  inversed_matrix <- NULL 
  
  #set <- function(y) sets matrix to object that one can create with makeCacheMatrix
  set <- function(y) {
    x <<- y
    inversed_matrix <<- NULL 
  }
  
  #get <- function() x can return (x) the input matrix like m <- matrix(1:4,2,2), x$get() returns this matrix
  get <- function() x 
  
  #set_inversion set the inversed_matrix using dummy inversed
  set_inversion <- function(inversed) inversed_matrix <<- inversed
  
  #get_inversion get the inversed_matrix 
  get_inversion <- function() inversed_matrix 
  
  #list containing a function to:
  #(1) set matrix to object 
  #(2) get inputed matrix
  #(3) set the inversed matrix 
  #(4) get the inversed matrix
  
  list(set = set, get = get,set_inversion = set_inversion, get_inversion = get_inversion)
}

## Write a short comment describing this function
#The following function inverses list created with the function makeCacheMatrix. 
#However, it first checks to see if the inversion has already been calculated. 
#If so, it gets the inversed matrix from the cache and skips the computation. 
#Otherwise, it calculates the inversion of the data and sets the values of the inversed matrix in the cache via the set_inversion function.

cacheSolve <- function(x, ...) {
  #x$get_inversion get the inversed matrix 
  m <- x$get_inversion() 
  #it first checks to see if the inversion has already been calculated
  #NULL if uncalculated due to inversed_matrix <- NULL,  
  if(!is.null(m)) { 
    message("getting cached data") #if already in cache
    return(m) #returns the inversion
  }
  #if inversion hasn't been calculated one can use x$get to get the inputed matrix
  data <- x$get() 
  #solving the square inputed matrix
  m <- solve(data) 
  #setting inversion
  x$set_inversion(m) 
  #returning result
  m 
}
