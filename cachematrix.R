## Put comments here that give an overall description of what your
## functions do:

#First function (makeCacheMatrix) creates a matrix that allows the caching of its inverse
##Second Function (cacheSolve) computes the inverse of the matrix returned by makeCacheMatrix. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   # Set the function, with x corresponding to a matrix, stored with the name "makeCacheMatrix"
         inverse <- NULL                      # Set the value of interest (inverse of the matrix) as null
  set <- function(y) {                        # Set the function with term "y", stored with the name "set"
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setSolve <- function(mean) m <<- Solve
  getSolve <- function() inverse
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}
}


## Write a short comment describing this function

##

cacheSolve <- function(x, ...) {            # Set the function, stored with the name "makeCacheMatrix"
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getSolve()
  if(!is.null(inverse)) {                   #Set the condition in which we get the message "getting cached data" and retrieve the inverse from the cache unless the inverse hasn't been calculated
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setmean(inverse)
  inverse
}
