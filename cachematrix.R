
## This function will cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }  ##this part stroes the key variables in a different environment
                get <- function() x
                setinverse <- function(solve) m <<- solve
                ## solving for the inverse of function x
                getinverse <- function() m
                ## creating a list 
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
                
        }



## This function retrieves the cached inverse of a matrix
## This avoids a large calculation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        }

