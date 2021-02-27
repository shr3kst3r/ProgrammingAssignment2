## The computing of the inverse of a matrix can be costly, so we implement
## two functions to allow for the caching and the solving of a matrix.

## == Example Usage ==

## The matrix we want to solve:
## > m <- matrix(c(7, 3, -2, 5), nrow = 2)
## > m
## [,1] [,2]
## [1,]    7   -2
## [2,]    3    5

## Make a cached matrix:
## > x <- makeCacheMatrix(m)

## Solve cached matrix (note the cached message on the second call):
## > cacheSolve(x)
## [,1]       [,2]
## [1,]  0.12195122 0.04878049
## [2,] -0.07317073 0.17073171
## > cacheSolve(x)
## getting cached data
## [,1]       [,2]
## [1,]  0.12195122 0.04878049
## [2,] -0.07317073 0.17073171


## We can verify that our solution matches another source (https://www.wolframalpha.com/input/?i=%7B%7B7%2C+-2%7D%2C+%7B3%2C+5%7D%7D):
## > all(matrix(c(5/41, -3/41, 2/41, 7/41), nrow = 2) == cacheSolve(x))
## getting cached data
## [1] TRUE

## == End of Example ==


## Takes a matrix and makes it a cacheable version.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Uses a cached matrix and returns the inverse of the cached matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()

    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)

    i
}
