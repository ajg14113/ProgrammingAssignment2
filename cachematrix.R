## CACHING AN INVERSE MATRIX
#  Finalized 01/19/18
#  For JHU Course 2, Week 2

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #initialized as an object to be used later
    set <- function(y) {
        x <<- y # superassignment to the ENCLOSING environment (e.g. PARENT or GLOBAL)
        i <<- NULL #clears any previous cache which could have been created before from makeCacheMatrix()
    }
    get <- function() x #value is retrieved from PARENT ENVIRONMENT, because x is not defined locally within get() 
    setinverse <- function(solve) i <<- solve #uses the solve function to calculate the inverse matrix
    getinverse <- function() i #get!
    
    list(set = set, get = get, #assigns each function as an element within a list(), returns to PARENT ENV
         setinverse = setinverse, #name vectors to enable use of $ in cacheSolve() as an EXTRACTOR
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve`  retrieves the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse() #attemps to retrieve an inverse from the object passed in as the argument
    if(!is.null(i)) { #checks to see if the result is NULL. If result is NOT NULL, we have a valid cache to return
        message("getting cached data, I'll retrieve that for you right away!")
        return(i)
    }
    data <- x$get() #if result IS NULL, we execute this piece of code. this line gets matrix from input object
    i <- solve(data, ...) #calculates the solved inverse matrix
    x$setinverse(i) #uses setinverse() on input object to set the inverse in the input object
    i #returns value of the inverse to the parent env by printing the object
}

## UNIT TESTS (BASED ON BELOW LINK)
# https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/

bmatrix = makeCacheMatrix(matrix(c(2,3,4,5), nrow=2, ncol=2)) #Example input

bmatrix$get()         # Returns original matrix
cacheSolve(bmatrix)   # Computes, caches, and returns matrix inverse

bmatrix$getinverse()  # Returns matrix inverse
cacheSolve(bmatrix)   # Returns cached matrix inverse using previously computed

bmatrix$set(matrix(c(1,7,93,62), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(bmatrix)   # Computes, caches, and returns new matrix inverse

bmatrix$get()         # Returns matrix
bmatrix$getinverse()  # Returns matrix inverse

## OTHER RESOURCES CONSULTED
# Great Guide - https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
# Basic superassignment explanation - https://stat.ethz.ch/pipermail/r-help/2011-April/275905.html
