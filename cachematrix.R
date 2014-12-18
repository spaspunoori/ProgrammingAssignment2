## We have two functions below. 
##  1. makeCacheMatrix to hold the value of a attribute in the memory  and its inverse too. passed in argument is type of matrix. 
##     we store matrix and inverse of the matrix into the memory. This funcation does not actually do inverse operation.
##     But, It marely holds the values in the memorey (cache).
##
##  2. cacheSolve funcation takes a matrix as input, checks the fucntion "makeCacheMatrix" get method to see if the given input 
##     already  have the inverse cached, if cached, it would return the value from cache, if not, it would perform the inverse
##     and store in the "makeCacheMatrix" fucation. for the next invocation, it would return from the cache.
## 

## makeCacheMatrix funcation to hold the values in cache (Memory) of x which is matrix type.

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  i<- NULL   ## asign a NULL to start with, inverse is NULL at the first.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setInverse <- function(solve) i <<- solve  ## when setInverse invoked, it would assing the value to "I"
  
  ## get the inverse of the matrix
  getInverse <- function() i  ## when getInverse Invoked, it would return the value of "i", which could be NULL
                              ## OR the Inverse of X which was cached.
  
  ## Below setter and getters are called whenever makeCacheMatrix been called
  
  list(set = set,                ## when set called, invoke set
       get = get,                ## when get called, invoke get  
       setInverse = setInverse,  ## similar to the get/set, but on the inverse side.
       getInverse = getInverse) 
}


## Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {

  i <- x$getInverse()  ## getInverse on X called, in this case, "makeCacheMatrix is called.
  
  ## check if there is the a value in "i", it could be NULL or a matrix.
  
  if(!is.null(i)) {   ## if not NULL then return it
    message("getting cached data")   
    return(i)
  }
  
  ## This part of the fuction only executes, if getInverse returned NULL 
  ## Here the funcation actually computing the inverse
  data <- x$get()
  i <- solve(data, ...)  ## asign the inverse to i
  
  x$setInverse(i)  ## set the inverse of the matrix
  i 

}
