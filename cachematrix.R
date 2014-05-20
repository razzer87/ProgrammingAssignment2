## The functions contained here aid in matrix calculations.

## The first function takes a matrix, then it creates an object that holds the 
## matrix and the function to get its inverse.

## The second function takes this object,  returns the inverse of the matrix and
## amends the object to store this result.

## If ran again on this object, it will return the stored inverse rather than 
## recalulating the result.

## This function takes a matrix and creates an object with it, the inverse 
## function and NULL

makeCacheMatrix <- function(x = matrix()) {
        ## m is defined and set to NULL to begin
        m <- NULL
        
        ## get will return the matrix
        get <- function() x
        
        ## setInverse contains the function to find the inverse of a matrix
        setInverse <- function(solve) m <<- solve
        
        ## getInverse will return what m has been set to.
        getInverse <- function() m

        list(get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function takes the object created above.
## It will set m to be the value of the "getInverse" element.
## If getInverse is NULL, it will calculate the Inverse of the matrix stored in 
## "set" and 

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
