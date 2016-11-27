## Put comments here that give an overall description of what your
## functions do
## The functions should set a matrix, calculate the inverse of
##this matrix and Cache it
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
## The purpose of this function is to initialize the object x
  ## x is initialized as a function and used to cache
  inv <- NULL
  set <- function(mat1){
    x <<- mat1
  }
  get <- function() x
  sInv <- function(inverse) inv <<- inverse
  gInv <- function() inv
  list(set= set, get = get, sInv = sInv, gInv = gInv)}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## x can be accessed by calling the makeCacheMatrix
  inv <- x$gInv()
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  mat2 <- x$get()
  inv <- solve(mat2, ...)
  x$sInv(inv)
  inv
}
