## Put comments here that give an overall description of what your
## functions do

## The following function makeCacheMatrix takes matrix as input. It has four functions set, get, setInverse and getInverse. 
## Function set: sets the given matrix to x; Function get: returns the matrix x.
## Function setInverse: takes in the value inverse and assigns to inv.
## Function getInverse: takes matrix and returns its inverse if available in the list.
## Thus following function creates a special "matrix" object that can cache its inverse if available.

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){
      x<<-y
      inv<<-NULL
      }
      get<-function(){x}
      setInverse<-function(inverse){inv<<-inverse}
      getInverse<-function(){inv}
      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The following function cacheSolve takes matrix as input.
## First it checks the availability of inverse of given matrix by calling getInverse function.
## If inverse is available then it will return the inverse of matrix with message "Getting Cached data".
## If inverse is not available then it will calculate the inverse using solve function and call setInverse function for storing it in inv.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
            message("Getting Cached data")
            return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}
