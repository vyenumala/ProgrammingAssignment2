## There are two functions for this code here that give an inverse a matrix without repeating the computationally intensive calculations every time
## The first function makeCacheMatrix creates a special "matrix" Object that would calculate inverse of a matrix 
## The assumption is that all matricies tested are invertible

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## This keeps the Cache clean if cacheSolve has not been run previously
        set <- function(y){
                x <<- y ## stores these values in cache and not in current environment for easy recall
                m <<- NULL
        }
        
        get <- function() x  ## get the matrix x 
        setmatrix <- function(solve) ## apply function solve to get inverse of matrix x
                m <<- solve ## save the inverse matrix to cache environment
        getmatrix <- function() m ## retrieve the inverse matrix m
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix) ## create a list which will be used as input for cacheSolve
}


## ## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
## Otherwise, it calculates and returns the new value


cacheSolve <- function(x =matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        } # it will retrieve the inverse matrix information if it is avaiable in cache
        
        matrix <- x$get() ## If it is a new matrix and no information available in cache, it will estimate inverse
        m <- solve(matrix,...)
        x$setmatrix(m) ## set the inverse matrix for the new matrix
        return(m)
}
