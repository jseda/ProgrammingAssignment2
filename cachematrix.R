## makeCacheMatrix is the fuctintion creating an object and calculated the inversion value of the matrix.
## In case there is the matrix already calculated the cached value is used instead of calculation
makeCacheMatrix <- function(x = matrix()) {

	# Initialization of cache to null
        invValX <- NULL
        
        # Store matrix
        set <- function(Y)
        {
	        x <<- y
	        invValX <<- NULL
        }
        
        # Get matrix
        get <- function() x
        
        # Calculate the inverse value
        setinverse<- function(inverse) invValX <<-inverse
        getinverse <- function() invValX
        
        # Return a list object
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix by function makeCacheMatrix.
## If the object is not stored in cached it is created and set to cached and retuned to calling context.
cacheSolve <- function(x, ...) 
{
	# Get the inverse value of 'x' if any
	invValX <- x$getinverse()
    
	# is there any object in cache?
	if (!is.null(invValX))
	{
		message("The cached inverse value is returned.")
        	return(invValX)
	}
	
	# Cache is empty, calculate the value and store it in cache
	message("Calculating a new inverse value and storing it in cache.")
	invValX <- solve(x$get())
        # Set the object into cache
        x$setinverse(invValX)
    
	return(invValX)
}
