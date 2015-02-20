## This is homeassignment2 for R_PROGRAMMING online course.
## Put comments here that give an overall description of what your
## functions do
## The following two functions will calculate the inverse of the matrix. 
## Because the computation of matrix is time consuming, the folloing function
## will cache the inverse instead of calculating it repeatedly. 
## The makeCachMatrix will cache the inverse and CachSolve function will calculate
## the inverse or retrieve it from the cache.
## when trying to call these two function, shold type:
## >cacheSolve(makeCacheMatrix(yourmatrix))

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## it does the following:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of matrix inverse
## 4. get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}



## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
