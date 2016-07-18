#makeCacheMatrix is a function that first empties the contents
#of the variables 'm' and  'n' by establishing that 'm' and 'n'
#are NULL. 
#This makes sure that if there was any cached data in 'm' and
#'n' prior to calling this function, that cached data is now
#emptied from those variables used in the function.
#makeCacheMatrix then takes the argument 'x', which is always
#a matrix, and caches it and its inverse in the variables
#'n' and 'm' respectively. 

#cacheSolve identifies whether the variable m has any cached 
#data in it.
#If m does have cached data within, the function will return m, 
#printing all the data inside m.
#If m does not have cached data, it will call the
#makeCacheMatrix function on the argument 'x' provided by the 
#cacheSolve function.
#'x' should always contain a square, invertible matrix, otherwise
#the function will not work.
#Because the cacheSolve function implements the makeCacheMatrix
#function, cacheSolve should be used in conjunction with 
#makeCacheMatrix, after makeCacheMatrix has been defined as a
#function.

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


cacheSolve <- function(x) {
        if(!is.null(m)) {
                return(m)
        } else {
                makeCacheMatrix(x)
                print(m)
                print(n)
        }
}
