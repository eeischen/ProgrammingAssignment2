## makeCacheMatrix creates an object that stores a matrix and caches its inverse.
##cacheSolve returns the inverse of the matrix returned by makeCacheMatrix.

## The following function returns a "matrix" object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
#Assign NULL to the inverse
inv<-NULL
  #define function that sets the matrix
  set<-function(y=matrix()){
    x<<-y
    inv<<-NULL
  }
  #define function that gets the matrix
  get<-function() x
  #define function that caches the inverse
  setinverse<-function(inverse) inv<<-inverse
  #define function that gets the inverse
  getinverse<-function() inv
  #return a list containing set, get, setinverse, and getinverse
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #retrieve the inverse from makeCacheMatrix
  inv<-x$getinverse()
  
  #if inverse has been cached, return inverse, and inform user that returning cached data
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  #otherwise, compute the inverse of the matrix
  theMatrix<-x$get()
  inv<-solve(theMatrix,...)
  
  #cache the inverse
  x$setinverse(inv)
  
  #return the inverse
  inv
}
