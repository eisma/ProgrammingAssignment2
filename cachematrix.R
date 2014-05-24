## Put comments here that give an overall description of what your
## functions do

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse


# The second function calculates the inverse of the special "matrix" created with the first function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
# of the inverse in the cache via the setinv function.


## Write a short comment describing this function --> inside the code

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # the cache is initialised empty
  set <- function(y) {
    x <<- y
    # the 1st function of the list: the original matrix is stored in the local environment
    m <<- NULL
    # create an empty cache also inside the local environment; 
    # otherwise the previous inverse is kept even if the matrix has been changed
  }
  get <- function() x
  # the 2nd function of the list: the matrix is delivered
  setinv <- function(solve) m <<- solve
  # the 3rd function of the list: the inverse is calculate via the solve function and stored in the cache variable m
  getinv <- function() m
  # the 4th function of the list: the inverse out of the cache is delivered
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  # the result is a list containing the four functions to set/get the original matrix and set/get the inverse
}


## Write a short comment describing this function --> inside the code

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  # the inverse is collected from the cache
  if(!is.null(m)) {
    # check if the cache wasn't empty, i.e. the inverse cached is different from null
    message("getting cached data")
    return(m)
    # if the inverse exists, print message, display inverse and leave the function
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  # if inverse does not exist: get the matrix, then calculate the inverse via solve, 
  # then give it back to setinv and display it
}

## Return a matrix that is the inverse of 'x'