## The two functions here define the capability to create and 
## display the inverse of a matrix, the capability will cache
## the value when applicable


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # create a variable to hold the value of the inverse
  # set inv to NULL
  inv <- NULL
  
  # create a setter function
  set <- function(y){
    x <<- y
    inv <<- NULL
    
  }
  
  # create a getter function
  get <- function() x
  
  # cache the inverse
  setInv <- function(inverse){
    inv <<- inverse
  }
  
  # return the cached inverse 
  getInv <- function(){
    inv
    
  }
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  

}


# cacheSolve: computes the inverse of a matrix made
# if the inverse is already calculated, the function will return the cached value

# Usage example
# x <- matrix(1:4, nrow=2, ncol=2)
# m <- makeCacheMatrix(x)
# s <- cacheSolve(m)
# print(s)
# s should return: 
#     [,1] [,2] 
#[1,]   -2  1.5 
#[2,]    1 -0.5 

# 
# s2 <- cacheSolve(m)
# displays "get the cached value"
# s2 should return: 
#     [,1] [,2] 
#[1,]   -2  1.5 
#[2,]    1 -0.5 

cacheSolve <- function(x, ...) {
  
  # get the inverse from cache
  inv <- x$getInv()
  
  # if inv is already cached, return the cache,
  if(!is.null(inv)){
    message("get the cached value")
    return(inv)
  }
  #return the inverse of the matrix if not cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
       
  
}
