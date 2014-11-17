## This file defines 2 functions that cache the inverse of a matrix. Matrix 
## inversion is a costly computation hence caching the inverse could mean a 
## computational benefit over repeatedly calculating the inverse for the 
## same matrix.

## makeCacheMatrix defines a matrix class with the following properties:
## - get(): retrieves the original matrix
## - set(): (re)sets the matrix to a new value and invalidates the inverse
## - getInverse() : retrieves the inverse matrix or NULL if not yet cached
## - setInverse() : sets the inverse matrix 
## 
## The function setInverse() is normally not called directly. To compute the 
## inverse matrix, the user has to invoke cacheSolve(), defined below. 
##
## Example:
## my.matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
## cacheSolve(my.matrix) # computes and caches the inverse in my.matrix
## my.matrix$getInverse()
## ## returns the inverse matrix
## my.matrix$get()
## ## returns the original input matrix
## my.matrix$set(another.matrix)
## my.matrix$getInverse()
## ## returns NULL because the matrix inverse is not yet computed

makeCacheMatrix <- function(input.matrix = matrix()) {
    # Note that the input argument to makeCacheMatrix will be cached as 
    # input.matrix. R does not require us (unlike C++ or Java) to define 
    # another local variable to cache the input argument. We do however need 
    # to define a local variable for the cached inverse matrix which we do here:
    cached.inverse <- NULL
    
    # set() function will cache a new input matrix and invalidate the 
    # cached inverse matrix
    set <- function(new.matrix) {
        input.matrix <<- new.matrix
        cached.inverse <<- NULL
    }
    
    # get() retrieves the cached input matrix
    get <- function() { input.matrix }
    
    # setInverse() is called from within cacheSolve() to set the calculated
    # inverse matrix
    setInverse <- function(calculated.inverse) { 
        cached.inverse <<- calculated.inverse 
    }
    
    # getInverse() retrieves the cached inverse matrix (which can be NULL if not
    # yet set by the function cacheSolve()!)
    getInverse <- function() { cached.inverse }
    
    # The result of makeCacheMatrix is a list of functions that can be used 
    # on a matrix created by makeCacheMatrix
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve operates on the cached matrix created by makeCacheMatrix. 
## It computes the inverse of the cached matrix and stores it back into the 
## object.
## The function will not recalculate the inverse if it has already been 
## calculated; in that case it will return the cached inverse matrix.
## See the example above for more details.

cacheSolve <- function(cache.matrix, ...) {
    # Has the inverse of the cached matrix already been calculated?
    inv <- cache.matrix$getInverse()
    if (!is.null(inv)) {
        message("Inverse has already been computed; getting cached inverse")
        return (inv)
    }
    
    # Now compute the inverse matrix. The original matrix is retrieved via 
    # cache.matrix$get(). We do NOT copy it into a local variable for 
    # performance reasons. Pass any extra variables to cacheSolve on to the 
    # solve() function (these extra variables are denoted by the ...).
    inv <- solve(cache.matrix$get(), ...)
    cache.matrix$setInverse(inv)
    
    # Also return the calculated inverse matrix
    inv
}

##
## Unit test
##
## Checks the correctness of the implementation
##
unitTest <- function() {
    # These are the tests that we will run. 
    # the inverse of input.matrix is inverse.matrix
    # the inverse of alternative.matrix is alternative.inverse.matrix
    sharedTests <- function(input.matrix, 
                            inverse.matrix, 
                            alternative.matrix,
                            alternative.inverse.matrix) {
        cached.matrix <- makeCacheMatrix(input.matrix)
        # Test 1
        if (!isTRUE(all.equal(input.matrix, cached.matrix$get()))) {
            print("Test 1 failed: $get()")
        }
        else {
            print("Test 1 passed: $get()")
        }
        # Test 2
        if (!isTRUE(all.equal(NULL, cached.matrix$getInverse()))) {
            print("Test 2 failed: $getInverse()")
        }
        else {
            print("Test 2 passed: $getInverse()")
        }
        # Test 3: calculate the inverse
        if (!isTRUE(all.equal(inverse.matrix, cacheSolve(cached.matrix)))) {
            print("Test 3 failed: cacheSolve()")
        }
        else {
            print("Test 3 passed: cacheSolve()")
        }
        # Test 4: retrieve the inverse
        if (!isTRUE(all.equal(inverse.matrix, cached.matrix$getInverse()))) {
            print("Test 4 failed: $getInverse()")
        }
        else {
            print("Test 4 passed: $getInverse()")
        }
        # Test 5: use the $set() function
        cached.matrix$set(alternative.matrix)
        if (!isTRUE(all.equal(alternative.matrix, cached.matrix$get()))) {
            print("Test 5 failed: $get()")
        }
        else {
            print("Test 5 passed: $get()")    
        }
        # Test 6: check if the inverse is NULL
        if (!isTRUE(all.equal(NULL, cached.matrix$getInverse()))) {
            print("Test 6 failed: $getInverse()")
        }
        else {
            print("Test 6 passed: $getInverse()")
        }
        # Test 7: cache the inverse
        if (!isTRUE(all.equal(alternative.inverse.matrix, 
                              cacheSolve(cached.matrix)))) {
            print("Test 7 failed: cacheSolve()")
        }
        else {
            print("Test 7 passed: cacheSolve()")
        }
        # Test 8: retrieve the inverse using getInverse()
        if (!isTRUE(all.equal(alternative.inverse.matrix, 
                              cached.matrix$getInverse()))) {
            print("Test 8 failed: $getInverse()")
        }
        else {
            print("Test 8 passed: $getInverse()")
        }
    }
    
    # Now run the tests
    matrix.1 <- matrix(c(1,0,0,0,1,-4,0,0,1), nrow = 3, ncol = 3)
    inverse.matrix.1 <- matrix(c(1,0,0,0,1,4,0,0,1), nrow = 3, ncol = 3)
    
    matrix.2 <- matrix(c(1:4), nrow = 2, ncol = 2)
    inverse.matrix.2 <- solve(matrix.2) # solve it rather than hardcoding
    
    # Run 1:
    print ("Run 1:")
    sharedTests(matrix.1, inverse.matrix.1, 
                matrix.2, inverse.matrix.2)
    # Run 2: switch the matrices
    print ("Run 2:")
    sharedTests(matrix.2, inverse.matrix.2, 
                matrix.1, inverse.matrix.1)
    
    print ("End of tests.")
}
