## Put comments here that give an overall description of what your
## functions do

##set of functions to cahe inverse of a matris

## Write a short comment describing this function

#function makecachematrix create special matrix with cache inverse
#this function includes get, set, getinverse and setinverse functions

makeCacheMatrix <- function(x = matrix()) {

invx <- NULL
      set <- function(y) 
	{
             x <<- y
             invx <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) invx <<- solve
      getinverse <- function() invx
      list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## Write a short comment describing this function
#function cacheSolve takes matrix as parameter
#checks for inverse and changes to the matrix
#if inverse exist it is return else inverse is computed and return

cacheSolve <- function(x, ...) {
      invx <- x$getinverse()
      if(!is.null(invx)) 
	{
             message("getting cached inverse Matrix")
             return(invx)
      }
      mat <- x$get()
      invs <- solve(mat, ...)
      x$setinverse(invx)
      invx

        ## Return a matrix that is the inverse of 'x'
}
