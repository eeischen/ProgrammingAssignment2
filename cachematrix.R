## Put comments here that give an overall description of what your
## functions do

## The following function returns a "matrix" object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
  set<-function(y=matrix()){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  theMatrix<-x$get()
  inv<-solve(theMatrix,...)
  x$setinverse(inv)
  inv
}
