## The two functions are used in order to save
## time when calculating inverses of matrices.
## Should the inverse be calculated, it is retrieved,
## thus saving time. If it has not been calculated
## yet, these functions do the job.

## MakeCacheMatrix creates a special object
## that can cache its inverse. It creates a list
## containing four functions.
## 1st: Sets the value of the matrix.
## 2nd: Gets the value of the matrix.
## 3rd: Sets the value of the inverse.
## 4th: Gets the value of the inverse.


makeCacheMatrix <- function(X = matrix()) {
        NULA <- matrix(0,dim(X)[1],dim(X)[2])
        m <- NULA
        set <- function(y) {
                X <<- Y
                m <<- NULA
        }
        get <- function() X
        setsolve <- function(mean) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function fetches the inverse matrix
## Should it be all zeroes because it has not 
## been calculated, then computes it. Fetches it
## otherwise.Finally returns the inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!all(m==0)) {
                message("getting cached solve")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}