## Put comments here that give an overall description of what your
## functions do

#The purpose of these functions is to utilize cached values for intensive
# calculations - such as inverse of matrix 

## Write a short comment describing this function

## The function "makeCacheMatrix" creates a matrix object that can cache its
##inverse
## It involves four functions -
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse matrix
# 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setInv <- function(Inverse) mInv <<- Inverse
  getInv <- function() mInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function
# The function  "cacheSolve" takes the above matrix created above
# tests if an inverse is already cached, if not, it calculates it and saves it
# in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      mInv <- x$getInv()
      if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
      }
      matrix_data <- x$get()
      mInv <- solve(matrix_data, ...)
      x$setInv(mInv)
      return(mInv)
}

# These functions were tested as below -

# 1. m1<-matrix(rnorm(100),4,4)
# 2. testM  <- makeCacheMatrix(m1)
# 3. cacheSolve(testM)
# 4. On re-running cacheSolve(testM)
# the following message is displayed "getting cached data"
