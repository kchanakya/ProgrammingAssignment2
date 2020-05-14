## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this creates a matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL ##initialize it to null
  set <- function(y) {  ## used to create a matrix
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x  ##used to retrive the matrix
  setinverse <- function(inverse) inv_matrix <<- inverse  ##sets the inverse value
  getinverse <- function() inv_matrix  ##used to retrive the inverse value
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse()  ##retrive the inverse
  
  is(!is.null(inv_matrix)) {  ##check whether it is cached
    message("getting cached data")
    return (inv_matrix)  ##if cached then return it
  }
  data <- x$get()  ##get the data
  inv_matrix <- solve(data,...)  ##solve for inverse
  x$setinverse(inv_matrix)  ##setting the inverse
  inv_matrix  ##inverse matrix
}
