# These will serve as functions that will cache the inverse of a matrix. 


# This function creates a special 'matrix' object that can cache its inverse. 
makeCacheMatrix <- function(a = matrix()) {
        
inverse <- NULL
        set <- function(b) { 
                a <<- b 
                inverse <<- NULL 
          }
        get <- function() a # Returning the matrix 
        setinverse <- function(inverse) inverse <<- inverse # Setting the inverse matrix 
        getinverse <- function() inverse # Returning the inverse matrix 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # Returning a list of functions
}


# This function computes the inverse of the special 'matrix' as mentioned above returned by the makeCacheMatrix. If the inverse has already been calculated, considering that the matrix has not changed, then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(a, ...) {
        # Return a matrix that is the inverse of 'a'
        inverse <- a$getinverse() 
        if (!is.null(inverse)) { # Return inverse if set otherwise for computation
                message("Getting cached data.")
                inverse
                } 
        matrix <- a$get ()
        inverse <- solve(matrix, ...) # Calculating the inverse
        a$setinverse(inverse) #Returning the inverse
        inverse
}

# Sample Test
test_a <- matrix(c(1,2,3,4),2,2)
test_a

test_b <- makeCacheMatrix(test_a)
cacheSolve(test_b) #inverse returned after computation

cacheSolve(test_b) #inverse returned from cache
