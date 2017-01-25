## makeCacheMatrix() - similar to makeVector(), except it caches an inverted matrix
## cachSolve() - similar to cachemean(), but uses the solve() function to create an inverted matrix

## similar to makeVector(), but stores an inversed matrix
## needs to get(), set() a matrix
## needs get- & set- campabilities synonymous to the inversion

makeCacheMatrix <- function(x = matrix()) {
    invertedMatrix <- NULL
    ## set() capabilities
    set <- function(newInput) {
        ## sets and caches the input variable
        x <<- newInput
        invertedMatrix <<- NULL
    }
    
    ## get() capabilities
    get <- function() x
    
    ## set() capabilities for inversion operation
    setInvertedMatrix <- function(invertedInput) invertedMatrix <<- invertedInput
    
    ## get() capabilities for inverstion operation
    getInvertedMatrix <- function() invertedMatrix
    
    ## list of function actions
    list(
    set = set,
    get = get,
    setInvertedMatrix = setInvertedMatrix,
    getInvertedMatrix = getInvertedMatrix
    )
}


## Verifies if an inverted matrix has been cached or creates one

cacheSolve <- function(x, ...) {
    ## retrive inverted matrix
    myInvertedMatrix <- x$getInvertedMatrix()
    if (!is.null(myInvertedMatrix)) {
        message("Getting cached data")
        return(myInvertedMatrix)
    }
    
    ## retrieve xted matrix, whether inverted or not
    myMatrix <- x$get()
    myInvertedMatrix <- solve(myMatrix, ...) ## solve() will invert the matrix, if possible
    x$setInvertedMatrix(myInvertedMatrix)
    myInvertedMatrix ## output
}
