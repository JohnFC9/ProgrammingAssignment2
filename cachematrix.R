## Put comments here that give an overall description of what your functions do:

  #First function (makeCacheMatrix) creates a matrix that allows the caching of its inverse
  #Second Function (cacheSolve) computes the inverse of the matrix returned by makeCacheMatrix. 

makeCacheMatrix <- function(x = matrix()) {      # Set the function, with x corresponding to a matrix, stored with the name "makeCacheMatrix"
  inverse <- NULL                                # Set the result of interest (inverse of the matrix) as null
  set <- function(y) {                           # Set the function with term "y", stored with the name "set" to assign a new matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x                            # Get matrix x
  setSolve <- function(Solve) inverse <<- Solve  # Set values of the inverse   
  getSolve <- function() inverse                 # Get inverse of matrix x
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

cacheSolve <- function(x, ...) {           # Set the function, stored with the name "cacheSolve" to return a matrix that is the inverse of 'x'
  inverse <- x$getSolve()                   
  if(!is.null(inverse)) {                  #Set the condition in which we get the message "getting cached data" and retrieve the inverse from the cache unless the inverse hasn't been calculated
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()                          # If the inverse hasn't been calculated, store  "get" function from x in a variable called "data"
  inverse <- solve(data, ...)              # Compute matrix inverse
  x$setSolve(inverse)                      # set the calculated inverse in the cache
  inverse                                  # Return inverse of x

}
