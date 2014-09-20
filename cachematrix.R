## x is a N X N matrix
makeCacheMatrix <- function(x = matrix()) { 
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function(){ x }
        setinverse <- function(inversematrix) inverse <<- inversematrix
        getinverse <- function(){ inverse }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x' 
cacheSolve <- function(x, ...) { 
	inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
