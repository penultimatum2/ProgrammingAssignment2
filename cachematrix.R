## For most matrices, finding the inverse is a costly computation.
## To avoid having to do this computation multiple times, we define
## functions to cache the inverse so it can easily be retrieved once
## it has been computed the first time. This code follows an example
## given by Roger D. Peng in his course R Programming.

## A function to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # The inverse of the matrix, initialized as NULL
        set <- function(matrix) {
                x <<- matrix # sets the underlying matrix
                inv <<- NULL # initializes the inverse as NULL
        }
        get <- function() x  # returns the underlying matrix
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
}


## A function that computes the inverse of a CacheMatrix and stores it.
## If the inverse has already been cached, it simply retrieves the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        # If the inverse is already cached, we simply retrieve it.
        if(!is.null(inv)) {
                print("getting cached inverse")
                return(inv)
        }
        # If the inverse isn't already cached, we have to compute it
        M <- x$get()        # get the underlying matrix for x.
        inv <- solve(M)     # compute the inverse
        x$set_inverse(inv)  # cache the inverse
        inv                 # return the inverse
}
