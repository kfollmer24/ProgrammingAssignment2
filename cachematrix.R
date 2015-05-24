## Using the function cacheSolve with makeCacheMatrix below
## takes a matrix as input, and produces the inverse of that matrix as output.
## An important additional capability of the functions below is that the matrix inverse
## is not re-calculated, if it already has been. To prevent this unneccessary step, the temporary
## storage technique of 'caching' is used.


# makeCacheMatrix creates a list with the results of taking the inverse of a matrix. The elements of the list
# can be referenced to determine if a rec
makeCacheMatrix <- function(x = matrix()) {
    # the output inverse matrix is initilized to null
    i <- NULL
    # The 'cache'-ing part. Expands the scope to the parent environment. 
    set <- function(y) {
      # stores the matrix
      x <<- y
      # Inverse is set to null when doing a new inverse calculation
      i <<- NULL
    }
    
    # returns the value of the evaluated expression x (aka the inverse)
    get <- function() x
    
    # calculates the inverse of the matrix
    setInverse <- function(solve) i <<- solve
    
    # gets the inverse
    getInverse <- function() i
    
    # keeps all of the previous results for reference
    list(set = set, get= get, setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve returns the inverse of the matrix of makeCacheMatrix.
# If the inverse = the original, then the inverse is not recalulated
# but the cached data is used

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  
  # If the matrix is not the same, the new matrix value must be read
  data <-x$get()
  
  # and the new inverse is calculated
  i <- solve(data, ...)
  
  # and the new inverse is stored in the cached data
  x$setInverse(i)
  
  #output the inverse
  i 
    
}

# test 1
A <- matrix(c(2,3,3,4),nrow = 2, byrow = TRUE)
y <- makeCacheMatrix(A)
cacheSolve(y)
cacheSolve(makeCacheMatrix(A))

#test 2
mat<-matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
y$set(mat)
cacheSolve(y)
