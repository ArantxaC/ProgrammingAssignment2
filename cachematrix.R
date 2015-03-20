## The first function, makeCacheMatrix, creates a special "matrix", which is really a list containing a function to:
## set the values of the matrix
## get the values of the matrix
## set the values of the inverse
## get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <- function(y){
        x <<- y
        inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inv <<- solve
  getmatrix <- function() inv
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## The second function, cacheSolve, calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix and sets the values of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrix()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setmatrix(inv)
    inv
}
