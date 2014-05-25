## makeVector creates a list object containing four functions that can 
## be used to cache the mean of a given vector. Once cached in the context of 
## makeVector, the value can be recalled using the getmean() function.
## This avoids re-running an expensive operation if it has already 
## been performed.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                ## When set() is first called, push the arg, 'y' into the 
                ## parent frame as 'x'
                x <<- y 
                ## Because no mean should exist at this point, make sure that 
                ## m is null in the parent frame.
                m <<- NULL 
        }
        ## get() returns the value of 'x' that was pushed into the parent frame
        ## of set() - the same in which get() is defined and hence, available
        ## to get to use. 
        get <- function() x 
        ## setmean() performs the caching of m when called from cachemean 
        ## for the first time.
        setmean <- function(mean) m <<- mean
        ## getmean returns m and is used by cachemean to determine whether
        ## the mean has already been generated. If it has, cachmean() will
        ## return that, otherwise, getmean() will return NULL and the mean
        ## will be generated in cachemean().
        getmean <- function() m
        ## The list of functions that is returned by makeVector() ready for 
        ## use in cachemean (or at the command line).
        list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
        ## get the value of 'm' from maekVector using the getmean() function
        ## that has been stored in the list created by makeVector(). If the 
        ## value is not NULL, return the value provided by getmean(). 
        ## Otherwise, leave the loop and calculate the mean for the first time.
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Use the get() function to grab the value of 'x' and push it into
        ## a numeric vector called data
        data <- x$get()
        ## calculate the mean of data and push it into a numeric vector called
        ## 'm'. At this stage, 'm' is local to cachemean() and would have to 
        ## be recreated if the function were run again.
        m <- solve(data, ...)
        ## calling setmean() with the local 'm' as an argument passes the 
        ## value back into the context of makeVector(). The setmean() function 
        ## is responsible for caching the mean for reuse.
        x$setmean(m)
        ## return m if it has been created for the first time. 
        m
}