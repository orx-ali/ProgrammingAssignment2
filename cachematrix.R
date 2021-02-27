#In this function you will find for a good solution to one of the most time consuming computation
#The function is about how to cache the inverse of the matrix 

#First we start with the defining of the matrix with the following function;


makeCacheMatrix <- function(value= matrix()) {
	  invvalue <- NULL

#set the matrix  and reset  the innvalue
	  set <- function(y) {
	              value <<- y
	              invvalue <<- NULL
	  }

#now  get matrix:

	  get <- function() value

#set inverse matrix
	  setinverse <- function(inverse)  invvalue <<- inverse

#get inverse matrix

	  getinverse <- function() invvalue
	
  list(set = set, get = get,
	       setinverse = setinverse,
	       getinverse = getinverse)
}


#Return the matrix that is inverse of value



cacheSolve <- function(value, ...) {
	        ## Return a matrix that is the inverse of ‘value’
	  inverse <- value$getinverse()

#Check for null
	  if(!is.null(inverse)) {
	    return(inverse)
	  }

#Return inverse if it is null
	  data <- value$get()
	  inverse <- solve(data, ...)
	  value$setinverse(inverse)
	  inverse
	  
	}
