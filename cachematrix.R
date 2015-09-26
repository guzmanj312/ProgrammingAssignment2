## makeCacheMatrix creates a list of four functions: set_matrix, get_matrix,
##set_Inverse, and get_Inverse. Furthermore, makeCacheMatrix attempts to coerce
##data into a matrix.

##set_Matrix allows the user to set the value of a matrix by subsetting the object where 
##the values for makeCacheMatrix are stored.

##get_matrix retrieves the data stored in the list created by createCacheMatrix
## and returns the matrix

##set_inverse allows the user to set a value for the inverse variable represented by "inv"

##get_inverse retrieves the value of the variable "inv" and returns it.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_Matrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  get_matrix <- function() x
  set_Inverse <- function(inverse) inv <<- inverse
  get_Inverse <- function() inv
  list(set_Matrix = set_Matrix, get_matrix = get_matrix, set_Inverse = set_Inverse, get_Inverse = get_Inverse)

}


## cacheSolve is a function that checks to see if there is a value for the
## inverse matrix from makeCacheMatrix function. If it exists then that value
## is retrieved from the cache and returned, otherwise the inverse matrix is 
##solved for by the "solve" function and returned.

cacheSolve <- function(x, ...) {
  inv <- x$get_Inverse
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get_matrix()
  inv <- solve(data, ...)
  inv
  
        ## Return a matrix that is the inverse of 'x'
}
