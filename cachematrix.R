## makeVector creates a special "vector", which is really a list containing a function to

makeVector <- function(x = matrix()) {
        m <- NULL
        ## set the value of the vector
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the vector
        get <- function() x
        ## set the value of the mean
        setmean <- function(mean) m <<- mean
        ## get the value of the mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## The following function calculates the mean of the special "vector" created with the above function

cachemean <- function(x, ...) {
        m <- x$getmean()
        ## checks to see if the mean has already been calculated
        if(!is.null(m)) {
                ## If so, it gets the mean from the cache and skips the computation
                message("getting cached data")
                return(m)
        }
        ## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        ## Return a matrix that is the inverse of 'x'
        m        
}
