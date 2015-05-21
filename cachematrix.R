## 
## 	This group of functions create an inverse matrix and cache 
## 	the result so that it can be returned from memory quickly
## 	vs. a cpu intensive and time consuming recalculation of the value 
## 

##
## makeCacheMatrix 
##   creates an object that contains a group of functions that you can use to 
##   get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	
	  ## if we are creating/assigning a new matrix then NULL the old inverse
        inverse <- NULL

	  ## create a set function that caches the matrix and resets its inverse
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

	  ## create a get function
        get <- function() {return(x)}

	  ## set the inverse in a higher environment that will be used as a cache
        setInverse <- function(i) inverse <<- i

	  ## create a get function for the inverse matrix
        getInverse <- function() {return(inverse)}

	  ## create a simple way to call the functions defined within this function
	  ## by storing them as name / value pairs within a list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##
## cacheSolve
##   calculates the inverse matrix 
##   This is the implementaiton of the "interface" that makeCacheMatrix defines
##   

cacheSolve <- function(x, ...) {     

	  ## call the input objects function to get the cached inverse matrix   
	  inverse <- x$getInverse()
        
	  ## if a cached value exists then just return it from the cache
	  if(!is.null(inverse)) {
                message("getting cached data")
        }
	
	  ## if a cached value does NOT exist, calculate it and cache it
	  else {
	          data <- x$get()
         	    inverse <- solve(data, ...)
        	    x$setInverse(inverse)
        }
	  return(inverse)
}
