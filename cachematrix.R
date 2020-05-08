## From what i understand the overall ibjective of this 2 functions is to act as an alternative to the solve function

## This function will create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i <<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



## This function will compute the matrix created previously
cacheSolve <- function(x, ...) {
  i=x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data=x$get()
  i=solve(data,...)
  x$setinverse(i)
  i
}
