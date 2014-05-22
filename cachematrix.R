## Cache the inverse of a matrix.
## These two functions store unique matrices and it's inverse in
## a data structure for retrival if found.

## Create a cache of consisting of a matrix and its inverse.  This
## is essentially a hash of matrix and its inverse result.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Calculate the inverse of a matrix if it has not been
## already been calculated.  If a matrix is found in the
## list, return it's inverse from the list; otherwise,
## calculate its inverse and store it in the list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(x)
  x$setinverse(m)
  m  
}

