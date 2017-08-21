
## Matrix inversion is usually a costly computation below functions cache inverse of a matrix to avoid recomputation 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setInverse<-function (MatrixInv) inverse<-MatrixInv
  getInverse <- function() inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of matrix returned by makeCacheMatrix above.If the inverse 
## has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse))
  {
    message("getting cached data")
    return(inverse)
  }
  data <-x$get()
  inverse<- solve(data)
  x$setInverse(inverse)
  inverse
}

