## The functions contained here aid in matrix calculations.

## The first function takes an invertible square matrix, then it creates a list 
## that holds the matrix and has a spot where it's inverse can go when calculated.

## The second function takes this list and stores the inverse of the matrix 
## if it hasn't been caclulated already.

## If ran again on this list, it will return the stored inverse element rather 
## than performing the operation again.



## This function takes a matrix and creates a 4 element list.
## One element is the matrix from the argument. Another is where the inverse will
## be stored. The others are functions for resetting the matrix and calculating
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## m is defined and set to NULL to begin
        m <- NULL
        
        ## set is a function that sets the matrix to be the argument.
        ## You can use this to change the value of the matrix in the object

        set <- function(y) {
                     x <<- y
                     m <<- NULL
                 
                ## e.g. If you had ran "cachedobject <- makeCacheMatrix(amatrix)" 
                ## and now you want to use bmatrix instead. 
                ## You do this by calling cachedobject$set(bmatrix).
        }
        
        ## get will return the matrix stored in the object
        get <- function() x
        
        ## setinverse contains the function to find the inverse of a matrix
        setinverse <- function(solve) m <<- solve
        
        ## getinverse will return what m has been set to.
        ## m is NULL as per line 21. Running cacheSolve sets this element to a value.
        ## If the matrix is changed using the set element, it reverts back to NULL.
        getinverse <- function() m

        ## Outputs a list that stores each of the functions defined above.
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function takes the object returned by makeCacheMatrix.
## It DOES NOT take the same argument as makeCacheMatrix.
## i.e. if you ran "cachedobject <- makeCacheMatrix(amatrix)" then here you run
## "cacheSolve(cachedobject)"
## The first time it runs, it calculates the inverse of the matrix and stores it
## in cachedobject$getinverse.
## If there is already value in getInverse from a previous call, it returns it 
## without further calculations.

cacheSolve <- function(x, ...) {
        
        ## sets m to be the value stored in the getinverse element
        m <- x$getinverse()
        
        ## Checks if getinverse is NULL
        if(!is.null(m)) { ##If it isn't null, it returns the value it stores
                message("getting cached data")
                return(m)
        }
        
        ## If it is NULL, it uses the get element to extract the matrix
        data <- x$get()
        
        ## it uses the solve function to find the inverse and sets it to m.
        m <- solve(data, ...)
        
        ## it uses the setinverse element to change getinverse to the inverse
        x$setinverse(m)
        
        ## Outputs the inverse it calculated
        m
}
