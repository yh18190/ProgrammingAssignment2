## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inversex <- NULL # Where the result of inversion is stored
      # A setter function is used to set a matrix to object created by makeCacheMatrix function
     
      set <- function(y) {
	  x <<- y
	  inversex <<- NULL # Initialises inversex to null
      }

      get <- function() x # return the input matrix
      setInv <- function(inv) inversex <<- inv # set the inversed matrix
      getInv <- function() inversex # return the inversed matrix
      # return a list that contains these functions, so that we can use
      # makeCacheMatrix object like these
      # x <- makeCacheMatrix(testmatrix)
      # x$set(newmatrix) # to change matrix
      # x$get # to get the setted matrix
      # x$setInv # to set the inversed matrix
      # x$getInv # to get the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)

}

##This function is used to calculate the inverse matric object obtained from makeCacheMatrix function
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInv() # get the inversed matrix from object x
      # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
      if(!is.null(m)) { # if the inversion result is there
	  message("Show cached data")
	  return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      m # return the  result
}


# Test
  # generate a random square, non-singular matrix
  test <- matrix(runif(9,1,100),3,3)
  # generate the makeCacheMatrix object with this matrix
  testCached <- makeCacheMatrix(test)
  # from now on calculate or retrieve calculated inversion using the cacheSolve function

  testInv <- cacheSolve(testCached)
  
