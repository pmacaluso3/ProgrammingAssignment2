## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Creates a list object that contains the matrix itself (mat), the cached inverse of mat,
##and a a cached version of mat. If repeatedly editing the matrix inside of a loop, making
##changes to x$mat only will let the cacheSolve function detect when the matrix has changed
makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  cachedInv <- NULL
  cachedMat <- NULL
  list(mat = mat, cachedInv = cachedInv, cachedMat = cachedMat)
}

## Write a short comment describing this function

##First, checks if both a cached inverse and a cached version of the matrix are available.
##If either is missing, calls solve() on x$mat and caches this inverse and mat. If both
##the cached inverse and the cached matrix are there, compares them to see if there has
##been a change since the last loop iteration. If there has been no change, returns the
##cached inverse. If there has been a change, recalculates the inverse and caches the
##inverse and mat.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.null(x$cachedInv) && !is.null(x$cachedMat)) {
    if(isTRUE(all.equal(x$mat, x$cachedMat))) {
      message("retrieving cached inverse")
      return(x$cachedInv)
    }
    else if(!identical(x$mat,x$cachedMat)) {
      message("matrix has changed, recalculating inverse")
      x$cachedMat <<- x$mat
      x$cachedInv <<- solve(x$mat)
      return(solve(x$mat))
    }
  }
  else {
    message("either cached inverse or older version of matrix is missing")
    x$cachedInv <<- solve(x$mat)
    x$cachedMat <<- x$mat
    return(solve(x$mat))
  }
}
    