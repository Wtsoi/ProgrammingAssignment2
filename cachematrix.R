## Caching the Inverse of Matrix

## Matrix inversion is usually time consuming and required repeated computation.

## Using R function to cache the inverse of a matrix might proof beneficiary to shorten
## the time consuming computation

## The first function ist to create a special "matrix" to cache its inverse by
## 1. set the value of the invertible matrix
## 2. get the value of the invertible matrix
## 3. set the value of the inverse
## 3. get the value ofthe inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <-NULL
    set <- function (y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function () x
    setInv <- function(inverse) Inv <<- inverse
    getInv <- function () Inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## The following function calculates the inverse of the special "matrix" returned by makeCacheMatrix above.
## If first checks to see if the inverse has already been calculated.
## If so, it takes the inverse matrix from the cache and skips the computation.
## Else, compute the inverse of the matrix and set the inverse values in the cache via  the setInv function

cacheSolve <- function(x, ...) {
    Inv <- x$getInv()
    if(!is.null(Inv)){
        message ("getting cached data")
        return(Inv)
    }
    Mtx <- x$get()
    Inv <- solve(Mtx, ...)
    x$setInv (Inv)
    Inv        ## Return a matrix that is the inverse of 'x'
}
