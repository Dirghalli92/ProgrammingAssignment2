#Adapted heavily from get/set $mean example on assignment page.
#Creates/takes and stores the inverse of a matrix into a cache, and
#returns the result


#First function 'makeCacheMatrix' creates/takes a matrix to store inverse
#Along with a list of functions to set/retrieve the values of a matrixand its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,get = get, setInv = setInv, getInv = getInv)
}

#Second function 'cacheSolve', checks to see if the inverse has
#already been created, computes the inverse, and returns the result

cacheSolve <- function(x, ...) {
        #Checks to see if inverse is available
        inv <- x$getInv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        #computes and returns the inverse if not available in cache
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInv(inv)
        inv
}