## These functions set the matrix and changes it into inverse.


makecachematrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
    
  }
  get <- function(){x}
  setinverse <- function(inverse) {inv<<-inverse}
  getinverse <- function(){inv}
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
  
}

cachesolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("get cached data")
    return(inv)
    
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Return a matrix that is the inverse of 'x'


## cache solve part changes the matrix into inverse

    
