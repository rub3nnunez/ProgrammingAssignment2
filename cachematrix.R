## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL ##Initialize the local matrix parameter
        
        
        ##Matrix management
        
        set <- function(y) {    ##set the matix
                x <<- y
                m <<- NULL
        }
        
        get <- function() x     ##get the matrix
        
        
        ##Reverse management
        
        setrev <- function(solve) m <<- solve   ##apply the reverse of the matrix
        
        getrev <- function() m                  ##retrieve the reverse value of the matrix
        
        list(set = set, get = get,
             setrev = setrev,
             getrev = getrev)
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getrev()      ##The reverse value is assigned
        
        if(!is.null(m)) {    ##Check caching
                
                message("getting cached data")
                return(m)
                
        }
        
        ##Not chached calculation applied
        
        data <- x$get()
        m <- mean(data, ...)
        x$setrev(m)
        m
        
}
