## Assignment 2 coursera course: Return a matrix that is the ##inverse of 'x'
## makeCacheMatrix : This function creates a special "matrix" ##object that can cache its inverse
## cacheSolve :  This function computes the inverse of the ##special matrix returned by `makeCacheMatrix` above.

makeCacheMatrix <- function(x = matrix()) { 
  ## This function creates a special "matrix" object that can     
  ## cache its inverse

  mtx<-NULL
  set<-function(y){
    x<<-y
    mtx<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) mtx<<- solve
  getmatrix<-function() mtx
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


cacheSolve <- function(x=matrix(), ...) {
  ## This function computes the inverse of the special
  ##   "matrix" returned by `makeCacheMatrix` above

  mtx<-x$getmatrix()
  if(!is.null(mtx)){
    message("getting matrix from cache")
    return(mtx)
  }
  matrix<-x$get()
  mtx<-solve(matrix, ...) ##Computing the inverse of a square matrix 
  x$setmatrix(mtx)
  mtx
}

