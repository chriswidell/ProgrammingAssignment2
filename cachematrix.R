## Since Matrix inversion is a costly procedure it has certain benefits of storing the inverse in the cache
## for easier usage. 

# makeCacheMatrix is a function that creates a list in the following 4 steps:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
        myInverse <- NULL
        set <- function(y) 
        {
                x <<- y
                myInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) myInverse <<- inverse
        getinverse <- function() myInverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix created by 
## makeCacheMatrix, otherwise retrieves it from the cache.
cacheSolve <- function(x, ...) 
{
        myInverse <- x$getinverse()
        if(!is.null(myInverse)) 
        {
                message("Retrieving cached data:")
                return(myInverse)
        }
        data <- x$get()
        myInverse <- solve(data)
        x$setinverse(myInverse)
        myInverse
}