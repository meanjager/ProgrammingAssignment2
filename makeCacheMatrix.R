## makeCacheMatrix creates a list object containing four functions that can 
## be used to cache the inverse of a given matrix as resolved by the solve()
## function. Once cached in the context of ## makeVector, the value can be 
## recalled using the getinverse() function.
##
## This avoids re-running an expensive operation if it has already 
## been performed.
##
makeCacheMatrix <- function(x, a, b) {
        ## Take the arguments provided and turn into a matrix
        x <- matrix(x, nrow = a, ncol = b)
        ## Make sure that matInv is initially NULL.
        matInv <- NULL
        set <- function(y) {
                ## When set() is first called, push the arg, 'y' into the 
                ## parent frame - the context of makeCacheMatrix()
                x <<- y 
                ## Because no mean should exist at this point, make sure that 
                ## i is null in the parent frame.
                matInv <<- NULL 
        }
        ## get() returns the value of 'x' that was pushed into the parent frame
        ## of set() - the same in which get() is defined and hence, available
        ## to get() to use. 
        get <- function() x 
        ## setinverse() performs the caching of m when called from cachemean 
        ## for the first time.
        setinverse <- function(inverse) m <<- inverse
        ## getmean() returns matInv and is used by cacheSolve() to determine if
        ## the inverse has already been generated. If it has, cacheSolve() will
        ## return that, otherwise, getinverse() will return NULL and the inverse
        ## will be generated in cacheSolve().
        getinverse <- function() matInv
        ## The list of functions that is returned by makeCacheSolve() ready for 
        ## use in cacheSolve (or at the command line).
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## get the value of 'matInv' from makeCacheMatrix() using the 
        ## getinverse() function that has been stored in the list created by 
        ## makeCacheSolve(). If the value is not NULL, return the value 
        ## provided by getinverse().
        ##
        ## Otherwise, leave the loop and calculate the mean for the first time.
        ##
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Use the get() function to grab the value of 'x' and push it into
        ## a new object called data
        data <- x$get()
        ## calculate the inverse of data and push it into 'matInv'. At this 
        ## stage, 'matInv' is local to cacheSolve() and would have to be 
        ## recalculated if the function were run again.
        matInv <- solve(data)
        ## calling setinverse() with the local 'matInv' as an argument passes 
        ## the value back into the context of makeCacheSolve(). The setinverse() 
        ## function is responsible for caching the mean for reuse.
        x$setmean(m)
        ## return matInv if it has been created for the first time. 
        matInv
}