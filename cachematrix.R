## makeCacheMatrix is the fuctintion creating an object and calculated the inversion value of the matrix.
## In case there is the matrix already calculated the cached value is used instead of calculation
makeCacheMatrix <- function(x = matrix()) {
        invValX <- NULL
        set <- function(Y)
        {
	        x <<- y
	        invValX <<- NULL
        }
        
        get <- function() x
        
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix by function makeCacheMatrix.
## If the object is not stored in cached it is created and set to cached and retuned to calling context.
cacheSolve <- function(x, ...) 
{
    ## Get the inverse value of 'x'
    invValX <- x$getinverse()
    if (!is.null(invValX))
    {
        message("The cached value is returned.")
        return(invValX)
    } else {
        invValX <- solve(x$get())
        x$setinverse(invValX)
    }
    
    return(invValX)
}
