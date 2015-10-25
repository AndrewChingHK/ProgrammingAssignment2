## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Author: Ching KA Chiu Andrew
## email: acching@gmail.com
## Userid: 9335556

## Description: makeCacheMatrix function is designed to initialize and return the matrix value
##              sub-function "get" to return the matrix value passed in argument
##              sub-function "setinverse" to set the values of inverted matrix to global valuable m
##              sub-function "getinverse" to return the value of inverted matrix
##              the list function is used to register the function to the list or function cannot be called 
##              directly outside the parent function,
##              
##              valuable m is desgined as global variable to save the value of inverted matrix
##              
##              cacheSolve function is designed to cache the value of inverted matrix to global variable
##              m, in case the object created by makeCacheMatrix is passed as parameter
##              Matrix will will processed to get inverted if not cached and saved in global variable m
##              The value will be returned without further processing once it saved in global variable m
##              The "solve" function is called to invert the matrix passing to makeCacheMatrix as argument
##
##              Type of arguments to function 
##                          - makeCacheMatrix - matrix
##                          - cacheSolve - object created by makeCacheMatrix
    
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(mat) m <<- mat
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("matrix inverse")
        message("getting cached data")
        return(m)
    }
    mdata <- x$get()
    m <- solve(mdata)
    x$setinverse(m)
    m
}
