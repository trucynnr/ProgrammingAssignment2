## Put comments here that give an overall description of what your
## functions do

##This is a script focused on the function makeCacheMatrix
##makeCachematrix consists of set,get,setInverse, getInverse, list

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL             ##initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}   ##function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##This is a script focused on caching the data

cacheSolve <- function(x, ...){      ##gets cache data
  inv <- x$getInverse()
  if(!is.null(inv)){                 ##check if inverse is null    
    message("getting cached data")
    return(inv)                      ##returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)             ##calculates inverse value
  x$setInverse(inv)
  inv                                ##return a matrix that is inverse of "x"  
}