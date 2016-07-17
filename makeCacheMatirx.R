makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        n <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        n <<- NULL
        }
        m <<- solve(x)
        n <<- x
}