## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL

    
    # set not inverse matrix matrix 
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    # get not inverse matrix
    get <- function() x

    # set the inverse
    setinverse <- function(inverse) inv <<- inverse

    # get the inverse
    getinverse <- function() inv

    # put into a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
    
    # get current state of the inverse and see if it has been computed yet
    inv <- x$getinverse()

    # If it has...
    if(!is.null(inv)) {
    	# return the computed inverse		
        message("Getting cached matrix")
        return(inv)
    }

    # If it hasn't...
    # get the matrix itself
    data <- x$get()

    # Find the inverse
    inv <- solve(data, ...)

    # Cache this result in the object
    x$setinverse(inv)

    # Return this new result
    inv    

        
        
}
