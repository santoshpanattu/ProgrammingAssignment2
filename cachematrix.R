## Write a short comment describing this function
## The first function, makeCacheMatrix returns a list of functions
## It stores a matrix and cached inverse of the matrix. This function contains
## set  - set the value of the Matrix
## get  - get the value of the Matrix
## setcacheInverse -  sets the chache with the inverse of the matrix
## getCacheInverse -  get the cached value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # initialize the cache 
  cache <- NULL
  
  #store the matrix
  set<- function(value) {
    x <<- value
    # clearing the cache as a new value is assigned
    cache <<- NULL 
  }
  
  # return the stored matrix
  get <- function() x
  
  # cache the matrix passed as an argument
  setCacheInverse <- function(mInverse) {
    cache <- mInverse
  }
  # return the stored cache 
  getCacheInverse <- function() cache
  
  #return the list 
  list(set=set,get=get, setCacheInverse=setCacheInverse, getCacheInverse = getCacheInverse)
  
}


## Write a short comment describing this function
##The following function calculates the mean of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setCacheInverse function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
   # get the cached value of the inverse
   inverse <- x$getCacheInverse
   #if cached value exists, then return the cached value
   if(!is.null(inverse)) {
     message("getting cached data")
     return(inverse)
   }
   #if not, get the matrix, calculate the inverse and store it in 
   # the cache
   data <- x$get()
   inverse <- solve(data)
   x$setCacheInverse(inverse)
   
   #return the inverse
   inverse
}
