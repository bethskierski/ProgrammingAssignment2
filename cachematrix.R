## makeCacheMatrix creates matrix and cacheSolve calculates its inverse

## makeCacheMatrix creates matrix by:
## setting value of matrix
## getting value of matrix
## setting value of inverse
## getting value of inverse


makeCacheMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function()i
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve calculates inverse of makeCacheMatrix
## if inverse has already been calculated, it retrieves it from the cache

cacheSolve <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("gettingcacheddata")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
