## The following functions are used when calculating the inverse of a matrix.
## Inverting a matrix can be time consumming. If we need to use the inverse
## of a matrix several times it would be benefitial if we could cache (store
## in memory) the result of the inverted matrix.

## Two steps are needed to get the results of an inverted matrix:
## 1) Check if the inversion has already been performed and stored
##      If the answer is yes then just retrive the result from the cache
##      don't do the inverion again. This is going to be performed by
##      the funtion called "makeCacheMatrix"
## 2) If the inversion is not stored then calculate the inverted matrix. This
##      is going to be performed by the funtion "cacheSolve"

## This function Check if the inversion has already been performed and stored
## If the answer is yes then just retrive the result from the cache.

makeCacheMatrix <- function(m = matrix()) {             ## Creates "m" to be an empty matrix
        
        im <- NULL
        
        set <- function(y) {                            ## This function set the value of the matrices
                m <<- y                                 ## "m" is the input matrix
                im <<- NULL                             ## "im" is the inverted matrix
                                                        ## "im" is initialized to be an empty matrix
        }
        
        get <- function() m                             ## This function gets (returns) the values of 
                                                        ## matrix "m"
        
        setsolve <- function(solve) im <<- solve        ## This function set the value of the inverse
                                                        ## matrix im
        
        getsolve <- function() im                       ## This function gets the values for the 
                                                        ## inverse matrix "im"
        
        list(set = set,                                 ## Makes a list of different matrices
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getsolve()                               ## Retrives getsolved from the list generated 
                                                        ## with makeCacheMatrix
        
        if(!is.null(m)) {                               ## If the stored inverse matrix exixts
                message("getting cached data")          ## retrives the stored inversed matrix
                return(m)
        }
        
        data <- x$get()                                 ## If inverse is not stored. Bring the original
                                                        ## matrix
        
        m <- solve(data, ...)                           ## Calculate the inverse with solve
        
        x$setsolve(m)                                   ## Store or cache the inverse of the matrix that was
                                                        ## just calculated
        m                                               ## Return a matrix that is the inverse
}

