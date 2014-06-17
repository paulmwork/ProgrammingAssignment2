## The makeCacheMatrix and cacheSolve functions work together to make a matrix inversion
# calculation run faster and with fewer computational resources if the inverse has already
# been calculated and the matrix has not changed.  It does this by using a cached value of the 
# matrix inversion calculation when the above conditions are met.



## The makeCacheMatrix function uses linear scoping to creat a matrix object that can cache
# a value for a matrix inversion.

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


## The cacheSolve function calculates the matrix inversion of the above-matrix and tests
# to see if the matrix inversion is unchanged and, if so, returns the previously calculated and 
# cached value thereof.

cacheSolve <- function(x, ...) {
            
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
