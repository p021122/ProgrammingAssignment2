## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a list containing set (set a matrix),
##get (call the matrix), setinverse (set the inverse of matrix)
##and getinverse (call the inverse of matrix) function 

makeCacheMatrix<-function(x = matrix()) {
  i<-NULL
  set<- function(y){
    x<<- y
    i<<- NULL
  }
  get<-function() x
  setinverse<- function(inverse) i<<- inverse 
  getinverse<- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes makeCacheMatrix(matrix) as an input
##and return inverse of the matrix (compute if  inverse of matrix is not cached
##or else restore from cache)

cacheSolve<- function(x,...){ #return a function that is inverse of x
  i<-x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<- solve(data,...)
  x$setinverse(i)
  i
}
