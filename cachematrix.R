## makeCacheMatrix:: takes matrix as argument
## validates mtraix as squared and saves the matrix and its inveverse
## set, get, getinverse and setinverse are inner functions
makeCacheMatrix <- function(m = matrix()) {
  ## Return a list contcaings the internal representationof cached matricx
  ## m is mtraics, im is inverse matrix stored in parent env
  if ( nrow(m) != ncol(m)) {
    message("This is matrix is not squared")
    return(NULL)
  }
  im <- NULL
  set <- function(y) {
    m <<- y
    im <<- NULL
  }
  get <- function() {
    m
  }
  setinverse <- function(inverse) {
    im <<- inverse
  }
  getinverse <- function() {
    im
  }
  ## return list with all the functions
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve::cacheSolve takes the modified matrix and returns the inverse
## On 1st call calculates the inverse and stores it in cache.  
## next call returnes the saved inverse from cache
cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- m$getinverse()
  if(!is.null(im)) {
    message("getting cached inverse")
    return(im)
  }
  data <- m$get()
  im <- solve(data,...)
  m$setinverse(im)
  im
}

## Test the functions
## Create own matrix
m1 <-matrix(1:4,2)
m1
mv1 <- makeCacheMatrix(m1)
mv1$get()
## Get Inverse - FIRST TIME
minv1 <- cacheSolve(mv1)
minv1
## 2ND CALL GETS IT FROM CACHE
minv2 <- cacheSolve(mv1)
minv2

## A*B = 1
m1%*%minv1
m1%*%minv2
