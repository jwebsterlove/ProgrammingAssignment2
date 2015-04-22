## Save matrix inverse compute time by caching inverses
## and retrieving rather than re-computing



## Create "matrix" that can cache its inverse

makeCacheMatrix <- function(mtoinvert = matrix()) 
{
     inv <- NULL
     setmatrix <- function(y) {
          mtoinvert <<- y
          inv <<- NULL
     }
     getmatrix <- function() mtoinvert
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(setmatrix = setmatrix, getmatrix = getmatrix,
          setinverse = setinverse,
          getinverse = getinverse)
     
     
}



## Get chached matrix inverse if exists, compute if not

cacheSolve <- function(x, ...) 
{
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$getmatrix()
     i <- solve(data, ...)
     x$setinverse(i)
     i
     
     
}
