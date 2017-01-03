## Here, we are calculating the inverse of an invertible matrix
## and cache the result. For the same matrix, the function does 
## not recalculate the inversion, rather returns the cached result.

## This function creates a list containing the matrix data,
## and a bunch of functions that enable to set or get the inversion
## of the matrix. 

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


## This function returns the inverse of a matrix. If 
## the inversion has already been done, the cached result will be 
## retrieved. Otherwise, the inverse is calculated. 

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
