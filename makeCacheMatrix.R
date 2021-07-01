## The functions involved in this activity aims to make use of makeCacheMatrix and cacheSOlve

## makeCacheMatrix function enables us to create a matrix which could cache its corresponding inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
                x <<- y
                inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse)inv<<-inverse
  getinv <- function(){
                inver <- ginv(x)
                inver%%x
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## On the other hand, cacheSolve function enables us to compute for the inverse of the matrix returned previously

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
                message("now getting cached data")
                return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv 
        ## Return a matrix that is the inverse of 'x'
}
