## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
     
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setcacheSolve <- function(cacheSolve) m <<- cacheSolve
                getcacheSolve <- function() m
                list(set = set, get = get,
                     setcacheSolve = setcacheSolve,
                     getcacheSolve = getcacheSolve)
        }

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            
                m <- x$cacheSolve()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- cacheSolve(data, ...)
                x$setcacheSolve(m)
                m
        }
}
