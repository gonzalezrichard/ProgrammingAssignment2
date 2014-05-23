## Overall description: the function "makeCacheMatrix" is actually a list of 
## functions that set a matrix, get it, set its inverse and get it. The function
## "cacheSolve" takes 

## This first function "makeCacheMatrix" takes an matrix as argument, and 
## returns a list of functions to set it, get it, set its inverse and get its
## inverse. It is assumed that the matrix passed as argument is invertible.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y) {    ## sets the values of x and m as free variables
            x <<- y
            m <<- NULL
      }
      get <- function() x     ## returns the value of x
      setinverse <- function(inverse) m <<- inverse   ## assigns the value  
            ## received as argument to the variable m
      getinverse <- function() m    ## returns the current value of m
      
      list(set = set, get = get, ## returns the list of functions
           setinverse = setinverse,
           getinverse = getinverse)
            
}


## The following function "cacheSolve" computes the inverse of the special list
## created with the first function. It first checks to see if the inverse has 
## already been computed. If it has, it gets the mean from the cache and skips 
## the computation. Otherwise, it computes the inverse of the data and sets the
## inversed matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      m <- x$getinverse() ## calls the method "getinverse" of the object "x" and
            ## assigns the value received (e.g. the inversed matrix) to the 
            ## variable m
      
      if(!is.null(m)) { ## if m is not null, it means that there is an inversed
                        ## matrix in the cache, already computed
            message("getting cached data")
            return(m)   ## prints a message to announce that the solution was not
                        ## computed but recovered from cache, returns the value
                        ## and exits
      }
      data <- x$get()   ## if m is null, there is no cached solution. Gets data
      m <- solve(data, ...)   ## from x, computes the inverse and stores it in m
      x$setinverse(m)   ## sets the inverse in object x to m
      m                 ## returns and exit
      
}
