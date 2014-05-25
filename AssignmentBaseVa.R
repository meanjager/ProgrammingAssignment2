makeVector <- function(x = numeric()) {
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
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        alist(set = set, get = get, setmean = setmean, getmean = getmean)
}

cacheMean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Use the get() function to grab the value of 'x' and push it into
        ## a vector called data
        data <- x$get()
        print(class(data))
        m <- mean(data, ...)
        x$setmean(m)
        m
}