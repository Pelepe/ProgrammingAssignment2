## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function defines a list of functions ## (set,get,setinverse,getinverse) over a matrix x.

makeCacheMatrix <- function(x = matrix()) {


## Initializes m variable as null
 m <- NULL

## It defines the set function that sets the matrix

  set <- function(y) {
    x <<- y
    m <<- NULL
  }

## It defines the get function that returns the matrix
  get <- function() x

## It defines the setinverse function that sets the inverse matrix
  setinverse <- function(inversematrix) m <<- inversematrix

## It defines the getinverse function that returns the inverse matrix
  getinverse <- function() m

## It returns the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve function returns the inverse matrix of x. 
##if the inverse has not been computed yet, the cacheSolve function computes it and returns it. 
##If it has been computed before it returns the cached inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 
## It calls the getinverse() function of x and assigns it to m
 m <- x$getinverse()

## If the inverse matrix has been computed before (m is not null) 
## the function cacheSolve returns the cached inverse matrix of x
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }

## If the inverse matrix has not been computed before, cacheSolve computes it, 
## sets it using setinverse() and finally returns it
  
  data <- x$get()
    
  m <- solve(data, ...)
    
  x$setinverse(m)
  m

}
