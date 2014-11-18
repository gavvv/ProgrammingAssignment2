## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		
        ## m is our inverse and this resets it to NULL
	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
	## returns value of original matrix
         get <- function() x
		
	## sets the value of the inverse
        setinverse <- function(inverse) m <<- inverse
		
	## gets the value of the inverse
        getinverse <- function() m
        
        ## This is a list of the internal functions ('methods') so a calling function
        ## knows how to access those methods. 
	list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	## access x and get inverse value
	m <- x$getinverse()
		 
	## checks to see if inverse has already been calculated and returns result if it has
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
    
	## gets data from x and calculates the inverse
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
