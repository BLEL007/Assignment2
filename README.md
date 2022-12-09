@@ -1,15 +1,37 @@
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                      # set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                       # get the value of the matrix
  setinv <- function(inverse) m <<- inverse #set the value of the inversed matrix
  getinv <- function() m                    #get the value of the inversed matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#            If the inverse has already been calculated (and the matrix has not changed), 
#            then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cachesolve <- function(x, ...) {
  m <- x$getinv()                   #gets the inverse of the matrix
  if(!is.null(m)) {                 #checks to see if the matrix has already been inversed
    message("getting cached data")
    return(m)
  }
  data <- x$get()                  
  m <- solve(data, ...)             #returns its inverse
  x$setinv(m)                       #sets the inversed matrix in the cache via the setinv function.
  m
}
