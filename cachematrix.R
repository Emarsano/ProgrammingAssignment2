
## makeCacheMatrix stores 4 functions (set, get, setinv,getinv)
## in order to set a (squared so invertible) matrix and its inverse
## "get" "getinv" "inv" "set" "setinv" "X" share the same environment    
## "cacheSolve" "makeCacheMatrix" are defined in the Global environment

makeCacheMatrix<- function(X = matrix()) {
        inv <- NULL
        set <- function(y) {
                X <<- y
                inv <<- NULL
        }
        get <- function() X
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve get the inverse matrix set in the 
## makeCacheMatrix function and check if it is NULL or not. 
## If it is not null, we get a message and the inverse matrix
## is retrieved from the makeCacheMatrix function. Otherwise,
## the inverse of X is computed and stored in makeCacheMatrix.

cacheSolve <- function(X, ...) {
        inv <- X$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- X$get()
        inv <- solve(data, ...)
        X$setinv(inv)
        inv
}
