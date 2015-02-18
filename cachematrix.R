##This program will use the a function to calculate the inverse of a matrix 
##and save it to the cache. Another function will provide capability use the
##cached value from previous calculation. Thus reducing the need to recalculate
##the inverese of a matrix.

## This function creates a matrix for calculation of an inverse matrix. It will
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
    ## initial cache
    m <- NULL
    
    ## assign input matrix and re-initialize cache in the parent environment
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    #return matrix
    get <- function() x
    #assign cache to the inverse of the input matrix
    setinverse <- function(inverse) m <<- inverse
    #return inversed matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function check if the calculation has been performed previously.
## If so, it will retrieve the calculation and skips the call to makeCacheMatrix()
## Otherwise it will make the call and set the cache.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## Check if calculation function has been previously run
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
