## Function makeCacheMatrix() cheates a special matrix 
## which is a list containing four functions:
## - function set accepts a regular matrix;
## - function get returns the matrix which was accepted earlier;
## - function setmatrix calculates the inverse matrix and populates the cache;
## - function getmatrix returns the cache (the inverse matrix or NULL);
## Along with list of functions it may contain a matrix
## and its inverse in cache, if it was calculated earlier.
##
## How to use:
## Assign this function to a variable.
## my_matrix <- makeCacheMatrix()
##
## Feed in a real matrix data structure.
## NOTE: Make sure the real matrix fed is inversable.
## my_matrix$set(matrix(c(2,0,0,2),2,2))

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y){
    x <<- y
    cache <<- NULL }
  
  get <- function() x
  setmatrix <- function(solve) cache <<- solve
  getmatrix <- function() cache
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Function cacheSolve() accepts a special matrix 
## created with help of makeCacheMatrix() function.
## It first checks the cache and returns previously 
## calculated inverse matrix if the cache is not empty.
## If the cache is empty, it solves the inverse matrix,
## saves it in the cache for future use and returns the inverse.
##
## How to use:
## Run the function first time with a special matrix
## create earlier:
## cacheSolve(my_matrix)
##
## Run the function with the same special matrix again
## to see if chached data is being used:
## cacheSolve(my_matrix)


cacheSolve <- function(x=matrix(), ...) {
  cache <- x$getmatrix()
  if(!is.null(cache)){
    message("getting cached data")
    return(cache) }
  
  matrix <- x$get()
  cache <- solve(matrix, ...)
  x$setmatrix(cache)
  cache
}
