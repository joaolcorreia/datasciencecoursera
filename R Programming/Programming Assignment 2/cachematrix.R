## This function creates a special "matrix" object that can cache its inverse.
## Inversable Matrix c=rbind(c(4, 3), c(3, 2)) 

makeCacheMatrix <- function(m = matrix()) {
        
        ## gets the value of the matrix
        get <- function() m
        
        ## sets the value 
        set <- function(y) {
                m <<- y
                ## Need to set the object null otherwise cacheSolve wont validate is.null
                mi <<- NULL
        } 
        
        ## get inverse matrix mi
        getinverse<-function() mi
        
        ## set inverse matrix mi on the global environment (cached)
        setinverse<-function(x) mi<<-x
        
        ## Makes the functions inside the makeCacheMatrix available
        list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
        ## Get matrix inverse
        mi <- m$getinverse()
        
        ## If the inverse matrix is not null we return it (mi) from global environment (cached)
        if(!is.null(mi)) {
                message("getting cached matrix")
                return(mi)
                
        } else {
                
                ## The inverse is null. We need to calculate it
                ## Get the main matrix
                x <- m$get()
                ## Calculate the inverse matrix
                inv <- solve(x)%*%x
                ## Set the inverse matrix
                m$setinverse(inv)
                
        }
        
}
