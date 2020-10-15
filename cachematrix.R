
## makeCacheMatrix() returns a set of functions as a list to the parent 
## environment.
## Objects x and m are initialized in the makeCacheMatrix environment.
## Object x as a function argument and an empty matrix and object m is set to 
## NULL.
## These two objects will hold information within makeCacheMatrix().

makeCacheMatrix <- function(x = matrix()) {  
    m <- NULL  
    
## The set function is the first function nested within the makeCacheMatrix
## environment.    
## Set() assigns the input argument,y, to the object x in the 
## makeCacheMatrix environment. This allows us to set the value of x even 
## after the object is created.
## This function also nullifies object m, removing any stored value that had 
## been cached from the previous run of cacheSolve().     
## The '<<-' operator ensures the assignment is made to the parent environment 
## and not within set().    
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

## get() retrieves the value of x from the parent environment.
## setinverse() allows for the assignment of the input argument, which in this 
## case is the calculated inverse from cacheSolve(), to the object m in the 
## parent environment. 
## getinverse() retrieves the value of object m from the parent environment.
## Finally makeCacheMatrix() returns a list of said functions. The functions  
## become elements of the list and are named to allow access with the $ form of 
## the extract operator.
    
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
    
}

## cacheSolve() calculates the inverse of object x, a matrix, defined in 
## the makeCacheMatrix() environment. It also checks if the inverse has  
## already been calculated. If the inverse has already been calculated, 
## it returns the inverse cached in object m without running solve().

cacheSolve <- function(x, ...) {

## The following lines of code retrieves the value of m from makeCacheMatrix().
## If m currently stores a value/matrix, cacheSolve will return its value. This
## indicates that the computation of the inverse had already been performed.
  
  
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
## If m is null, the value of x from makeCacheMatrix() is retrieved, solved,
## returned and cached or assigned to object m in makeCacheMatrix(). 
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
  
