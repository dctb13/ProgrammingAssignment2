# Solution to programming assignment 2, two functions required are below
# First function 'makeCacheMatrix' will create matrix
# The second function will return the inverse of the matix and cache the result

# function to create a special Matric and cache its inverse
makeCacheMatrix <- function (x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solve) i <<- solve
        
        getInverse <- function() i
        
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

# function to retrieve cached data
cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()
        
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data)
        
        x$setInverse(i)
        
        i
}