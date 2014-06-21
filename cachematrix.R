#############################################################################
## FUNCTION : makeCacheMatrix()
## ACTION   : Create an list (vector), object that set the value of a matrix 
##            and his inverse  in the cache (in memory).
## PARAMETERS: x is a square invertible matrix 
##            (example: c <- makeCacheMatrix(matrix(c(2,4,6,8),ncol=2)))
## OUTPUT    : Return a list containing a function 
##            to:
##            -   set a matrix in memory
##            -   get the matrix 
##            -   set the inverse of the matrix in memory
##            -   get the inverse of the matrix
##############################################################################
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversa <- function(solve) m <<- solve
  getinversa <- function() m
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}

#############################################################################
## FUNCTION : cacheSolve()
## ACTION   : Calculates the inverse of the list (vector) created with the 
##            function makeCacheMatrix(x) improving the computed time  
##            retrieving the inverse matrix from the cache (if the content
##            of the original matrix not changing and if the inverse matrix  
##            is store in the cache )
##            - if <exist the inverse matrix in cache>
##              then        
##                  <display a message "getting cached data"> 
##                  <return the inverse matrix stored in cache
##                   without calculations>
##              else
##                  <calculate the inverse of a matrix (function solve)> 
##                  <store the inverse in cache>
## SAMPLER:    Using the variable c used in example of makeCacheMatrix
##              >  cacheSolve(c) #return inverse of matrix(c(2,4,6,8),ncol=2)
##
## PARAMETERS: x is a square invertible matrix 
##            (example:  matrix(c(2,4,6,8),ncol=2))
## OUTPUT    : display the inverse matrix
##############################################################################
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinversa()
  if(!is.null(m)) 
  {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversa(m)
  m
}
