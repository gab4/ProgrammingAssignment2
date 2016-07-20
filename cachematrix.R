#makeCacheMatrix is a function that first empties the contents
#of the variable 'm' by establishing the 'm' is NULL in the set
#function.
#This makes sure that if there were any cached data in 'm'
#prior to calling this function, that cached data is now
#emptied from said variable, 'm', used in the function.
#makeCacheMatrix then takes the argument 'x', which is always
#a matrix, and extracts it using the get function. 
#The setinv function then caches the character vector 'inv'
#within 'm' ('inv' later contains the inverse of 'x' when the
#cacheSolve function is called).
#The getinv function extracts the variable 'm' and then a list
#of all the functions set, get, setinv and getinv is created
#at the bottom.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#cacheSolve identifies whether the variable m has any cached 
#data in it.
#If m does have cached data within, the function will return 'm' 
# with all the data inside 'm'.
#cacheSolve then calls the get function from makeCacheMatrix and
#stores the matrix 'x' within 'data'. 
#The solve function is used to get the inverse of 'data' and store
# the inverse within 'm'.
#Calling the setinv function from makeCacheMatrix, cacheSolve then
#caches the inverse inside 'm'.


cacheSolve <- function(x) {
        m <- x$getinv()
        if(!is.null(m)) {
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
