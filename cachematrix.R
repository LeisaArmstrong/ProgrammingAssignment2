## cacheMatrix.R
##Assignment 2 R Programming Caching the Inverse of a Matrix 

## 19 March 2015 
## Leisa Armstrong 
## Overview 
##This second programming assignment will require you to write an R function is able 
##to cache potentially time-consuming computations. For example, taking the mean of 
##a numeric vector is typically a fast operation. However, for a very long vector, 
##it may take too long to compute the mean, especially if it has to be computed 
##repeatedly (e.g. in a loop). If the contents of a vector are not changing, 
##it may make sense to cache the value of the mean so that when we need it again,
##it can be looked up in the cache rather than recomputed.

## This function creates a special matrix object that can cache an inverse 

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix function. 
## In the inverse has already been calcultated (and the matrix has not changed)
## then the cacheSolve function should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
