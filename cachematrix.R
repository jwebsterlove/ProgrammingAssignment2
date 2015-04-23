## Save matrix inverse compute time by caching inverses
## and retrieving rather than re-computing



## Create "matrix" that can cache its inverse

makeCacheMatrix <- function(mtoinvert = matrix()) 
{
     #clear inverse if defining a new "matrix" (list containing the 4 functions)
     inv <- NULL
     
     #replace current matrix and clear old inverse, in makeCacheMatrix environment
     setmatrix <- function(y) {
          mtoinvert <<- y
          inv <<- NULL
     }
     
     #return current matrix
     getmatrix <- function() mtoinvert
     
     #assign specified matrix to inverse in makeCacheMatrix environment
     setinverse <- function(inverse) inv <<- inverse
     
     #return matrix inverse
     getinverse <- function() inv
     
     #function output - list of four elements, all functions for the input matrix
     list(setmatrix = setmatrix, getmatrix = getmatrix,
          setinverse = setinverse,
          getinverse = getinverse)
     
     
}



## Get chached matrix inverse if exists, compute if not

cacheSolve <- function(x, ...) 
{
     ## Return a matrix that is the inverse of 'x'
     
     #call getinverse() function of input list x
     i <- x$getinverse()
     
     #if have previously calculated inverse of x's matrix, retrieve it
     if(!is.null(i)) {
          message("getting cached data")
          #output inverse and exit function
          return(i)
     }
     
     #otherwise compute inverse now
     #retrieve matrix
     data <- x$getmatrix()
     #compute inverse
     i <- solve(data, ...)
     #"cache" inverse
     x$setinverse(i)
     #output inverse
     i
     
     
}
