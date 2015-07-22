## This function creates a special "matrix" object that can cache its inverse
## Special "matrix" contains 4 function, which can be used by another function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    # m - cache of inverse matrix. first run - clear cache
        set <- function(y) {   # set new main matrix, clear cache of inverse 
                x <<- y     
                m <<- NULL  
        }
        get <- function() x    # get main matrix
        setinverse <- function(inverse) m <<- inverse    # set cache of inverse
        getinverse <- function() m    # get inverse matrix from cache
        
        # Use list() to store the 4 functions in the special "matrix"
        list(set = set, get = get,setinverse = setinverse,
             getinverse = getinverse) 
        
}


## This function computes the inverse matrix 
## by functions from object, created by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() # get inverse matrix from cache
        
        # if cache of inverse matrix exists then use it as result
        if(length(m)!=0) {
                message("getting cached data")
                return(m)
        }
        # if cache of inverse matrix doesn't exist 
        # then compute it, store in the cache and return result
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
