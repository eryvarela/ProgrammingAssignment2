## The first function makeCacheMatrix will create a special matrix.
## This matrix is invertible. Then get the value of the inverse, 
## and create a list.

makeCacheMatrix <- function(x = matrix()){ 
      inv <- NULL
      set <- function(y){
            x<<- y
            inv<<- NULL
      }
      get<- function(){x}
      setInverse<- function(inverse) {inv<<- inverse}
      getInverse<- function() {inv}
      list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
      }

## The second function computes the inverse of the matrix created, and assigned it to inv.
## If the inverse has already been calculated it can get the inverse from the cache, otherwise 
## it would be required to compute the inverse of the matrix and set the value
## of the inversion.

cachesolve<- function(x,...){
      inv<- x$getInverse()
      if(!is.null (inv)){
            message ("getting cached data")
            return(inv)
      }
      mat<- x$get()
      inv<- solve(mat,...)
      x$setInverse(inv)
      inv
}
