## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y){
        x <<- y
        I <<- NULL
    }
    
    get <- function() x
    set_inverse <- function(inv) I <<- inv
    get_inverse <- function() I
    list(set = set, get = get, 
         set_inverse = set_inverse, get_inverse = get_inverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)){
            message("get cached matrix inverse")
            return(inv)
        }
        data_matrix <- x$get()
        inv <- solve(data_matrix)
        x$set_inverse(inv)
        inv
}
