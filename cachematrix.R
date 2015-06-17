## The first function (makeCacheMatrix) creates a matrix for testing.
## The second function (cacheSolve) calculates the inverse of the test 
## matrix (if possible) or recalls, it if done previously.

## makeCacheMatrix creates a matrix for testing.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) { ## sets the test matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## recalls the test matrix
  setinverse <- function(inv) { ## sets inverse of the test matrix
    if(nrow(x) == ncol(x)) {
      inv <<- solve(x) 
    }
  }
  getinverse <- function() inv ## recalls the inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Computes the inverse of the test matrix (if square) or recalls 
## it if available.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) { ## looks for a recorded inverse first
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if(ncol(data)==nrow(data)) { ## only compute inverse if x is square
    inv <- solve(data)
    x$setinverse(inv)
  }
  inv
}
