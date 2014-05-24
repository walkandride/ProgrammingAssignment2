## Cache the inverse of a matrix.
## These two functions together:
## 1.  store unique matrix-matrix inverse pairs in a hash-map type
##     data structure
## 2.  calculate a matrix inverse

## Create a cache of consisting of a matrix and its inverse.  This
## implementation is essentially a hash (e.g. map, associative array)
## consisting of a matrix [as the key] and its inverse [as the result].

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL				# initialize
		set <- function(y) {	# set the value 
				x <<- y
				m <<- NULL
		}

		get <- function() x		# get the value 
		setinverse <- function(inverse) m <<- inverse	# set value of matrix
		getinverse <- function() m	# get value of matrix inverse
		list(set = set
			, get = get
			, setinverse = setinverse
			, getinverse = getinverse)
  
}


## Calculate the inverse of a matrix if it has not been
## already been calculated.  If a matrix is found in the
## list (defined in the makeCacheMatrix function), return 
## it's inverse from the list; otherwise,
## calculate its inverse and store it in the list before
## returning the result.

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()	# get matrix inverse from list
		if(!is.null(m)) {
				message("getting cached data")
				return(m)
		}
		
		data <- x$get()
		m <- solve(x)	# perform matrix inverse operation
		x$setinverse(m)
		m  
}
