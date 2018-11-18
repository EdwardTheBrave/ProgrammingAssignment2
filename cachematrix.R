## With this functions you get an easy way of creating the inverse of your matrixes and storing it in the cache so you can save some calculation time.
## The first function is basically an auxiliary function of getters and setters.With the second one you just calculate de inverse of a matrix and stores it in the cache.
## When calling that function again, if the result is in the cache it'll show it from there, saving some time.

## This is the function that creates the getters and setters for later calculations.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This is the function that creates the inverse of a matrix and stores it in the cache to use it later in case the result was already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' whether it can take it from
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


# Here there are two examples to run in case you want to give it a try!

m0 <- matrix(1:4, 2, 2)
m1 <- makeCacheMatrix(m0)
cacheSolve(m1)

#calling it again so it can take it from the cache
cacheSolve(m1)

m0.2 <- matrix(c(1,1,0,1,0,1,0,1,0), 3, 3)
m1.2 <- makeCacheMatrix(m0.2)
cacheSolve(m1.2)

#calling it again so it can take it from the cache
cacheSolve(m1.2)