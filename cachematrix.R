## This set of functions will create a matrix that can cache its inverse so 
## that the inverse does not need to be repeatedly calculated.

## makeCacheMatrix creates a special matrix object that can cache its inverse.  Returns list of functions.

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set <- function (y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve takes a makeCacheMatrix object and returns the inverse (either computed for the first time 
## or returned from the cache if previously computed)

cacheSolve <- function(x, ...) {
    inv<- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}
