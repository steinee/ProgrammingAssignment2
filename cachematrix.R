## Put comments here that give an overall description of what your
## functions do
## These two functions demonstrate the lexical scoping rules of R. The first function, makeCacheMatrix, defines,
## in essence, an interface to a function that computes the inverse of an invertible matrix. NOTE: no
## checks are done to ensure the matrix is invertible. MakeCacheMatrix keeps a copy of the inverted matrix and
## retrieves the copy rather than computing the inverse. If the matrix is changed, then the inverse is
## calculated and the inversed saved for future use.


## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. The key is tht the set()
## function initializes the stored inverse to NULL so tht its companion function knows when to recalculate 
## or merely retrieve the previously stored inverse.

## The stored invers is referenced by "inv" from the functions defined here using the <<- operator to reach
## the parent environmnet of the functions.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(invx) { inv <<- invx }
  getinv <- function(){ inv }
  list(set = set, # This list contains the functions for getting/setting the matrix and its inverse.
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache. If the
## matrix is changed, then the caompanion function, makeCacheMatrix will set the inverse to NULL so that 
## cache will recompute the inverse.

cacheSolve <- function(cachex,...){
  inv <- cachex$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # cached data does not exist, so compute the inverse and save it.
  data <- cachex$get()
  inv <- solve(data, ...)
  cachex$setinv(inv)
  return(inv)
}