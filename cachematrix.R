## With this functions you get an easy way of creating the inverse of your matrixes and storing it in the cache of the first function.
## Whenever you run the second function, it'll look at the cache of the first one to see if it can speed things up by using that value instead of calculating it on each call.
## If there's no such value in the cache, it'll calculate it itself.

## This is the function that creates and stores a matrix and its inverse for later calculations.

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


## This is the function that looks for the inverse of a matrix in the cache of the previous function, and if it doesn't find it, calculates it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' whether it can take it from
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


# Here there are two examples to run in case you want to give it a try!

example_matrix_1 <- matrix(1:4, 2, 2)
cacheSolve(makeCacheMatrix(example_matrix_1))

example_matrix_2 <- matrix(c(1,1,0,1,0,1,0,1,0), 3, 3)
cacheSolve(makeCacheMatrix(example_matrix_2))

