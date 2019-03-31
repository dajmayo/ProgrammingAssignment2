# These will serve as functions that will cache the inverse of a matrix. 


# This function creates a special 'matrix' object that can cache its inverse. 
makeCacheMatrix <- function(a = matrix()) {
        
inverse <- NULL
        set <- function(b) {
                a <<- b 
                inverse <<- NULL 
          }
        get <- function() a 
        setinverse <- function(inverse) inverse <- inverse 
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special 'matrix' as mentioned above returned by the makeCacheMatrix. If the inverse has already been calculated, considering that the matrix has not changed, then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(a, ...) {
        # Return a matrix that is the inverse of 'a'
        inverse <- a$getinverse()
        if (!is.null(inverse)) {
                message("Getting cached data.")
                inverse
                }
        matrix <- a$get ()
        inverse <- solve(matrix, ...)
        a$setinverse(inverse)
        inverse
}

# Sample Test
test_a <- matrix(c(1,2,3,4),2,2)
test_a

test_b <- makeCacheMatrix(test_a)
cacheSolve(test_b) #inverse returned after computation

cacheSolve(test_b) #inverse returned from cache
