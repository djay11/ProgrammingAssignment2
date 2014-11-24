## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix is a function that defines a special vector
# with functions set, get, getinv and setinv.

makeCacheMatrix <- function(x = matrix()) {

i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function
# cacheSolve is a function that looks at the cache. If cache is empty it
# calculates the inverse of the matrix and diaplys it. If the inverse is cached
# it displays the message "getting cached data" and then displays the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        d <- x$get()
        i <- solve(d, ...)
        x$setinv(i)
        i
}

# Below is the testing of these functions to get intended output
# > source("C:\\Users\\dhananjay\\Desktop\\Data Science\\R Programming\\getCacheMatrix.R")
# > x = matrix(c(1,2,3,4), nrow=2, ncol=2)
# > z = makeCacheMatrix(x)
# > z$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(z) this is the first run that calculates inverse
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(z) this is second run that gets the inverse from cache
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
