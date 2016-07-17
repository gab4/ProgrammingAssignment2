cacheSolve <- function(x) {
        if(!is.null(m)) {
                return(m)
        } else {
                makeCacheMatrix(x)
                print(m)
                print(n)
        }
}