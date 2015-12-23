##------------- R Programming - Assignment #2 -------------- 
## 
## The functions makeCacheMatrix() and cacheSolve() work in tandem 
## to provide a mechanism for caching the result of the expensive 
## operation of finding the inverse of a matrix. 
## 
##---------------------------------------------------------- 
## 
## makeCacheMatrix() generates a "list" of set/get functions 
## to maintain a matrix ("mat") and its inverse ("inv"). 
## The return object acts as an encapsulated "Matrix" object with its 
## inverse embedded/cached and having "getter" and "setter" methods. 
## 
## Note: Even the "setter" methods return an object implicitly.  
## 
makeCacheMatrix <- function(mat = matrix()) 
{
    inv         <- NULL 
    list(set    = function(m) { inv <<- NULL; mat <<- m }, 
         get    = function()  mat,
         setInv = function(i) inv <<- i,
         getInv = function()  inv) 
}

## 
## cacheSolve() takes a "Matrix" object ("x") created by the makeCacheMatrix() 
## function and returns an inverse matrix of "x". 
## If the inverse has already been computed, it returns that. If not, then it computes 
## the inverse, caches it and returns it. 
## It leverages the getter / setter methods of the "Matrix" object to cache the inverse. 
## It also takes any additional paramaters that solve() accepts and passes it along. 
## 
## This is a compact implementation, which could easily be expanded for better readability  
## if needed. It leverages the fact that setInv() implicitly returns the inverse. 
## 
cacheSolve <- function(x, ...) 
{
    if (is.null(inv <- x$getInv()))  x$setInv(solve(x$get(), ...)) 
    else                             inv  
}
