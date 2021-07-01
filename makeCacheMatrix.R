## The functions involved in this activity aims to make use of makeCacheMatrix and cacheSOlve

## makeCacheMatrix function enables us to create a matrix which could cache its corresponding inverse

makeCacheMatrix <- function(x = matrix()) {
  inv.vill <- NULL 
  set.vill <- function(y) {
                x <<- y
                inv.vill <<- NULL
  }
  get.vill <- function()x
  setinv.vill <- function(inverse)inv.vill<<-inverse
  getinv.vill <- function(){
                inver <- ginv(x)
                inver%%x
  }
  list(set.vill = set.vill, get.vill = get.vill, setinv.vill = setinv.vill, getinv.vill = getinv.vill)
}

## On the other hand, cacheSolve function enables us to compute for the inverse of the previously returned matrix

cacheSolve <- function(x, ...) {
  inv.vill <- x$getinv.vill()
  if(!is.null(inv.vill)){
                message("now getting cached data")
                return(inv.vill)
  }
  data.vill <- x$get.vill()
  inv.vill <- solve(data.vill,...)
  x$setinv.vill(inv.vill)
  inv.vill
        ## Return a matrix that is the inverse of 'x'
}
