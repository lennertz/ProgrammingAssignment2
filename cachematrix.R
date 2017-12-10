## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
      cm <-NULL
      set <- function(y) {
          x <<-y
          i <<- NULL
       }
       get <- function() x
       setinv <- function(inverse) i <<- inverse
       getinv <- function() i 
       list(set = set, 
            get = get,
            setinv = setinv,
            getinv = getinv)
}


## CacheSolve Computes the inverse of the matrix above 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if (!is.null(i)) {
          message("data is already cached")
          return(i)
      }
      dat <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
      }
