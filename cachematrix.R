## This function creates a matrix that is able to cache it's inverse.
## It seems to be making an object like an object oriented language

makeCacheMatrix <- function(x = matrix()) { 
        
        inversedMatrix <- NULL 
        
        set <- function(y) { 
                x <<- y 
                inversedMatrix <<- NULL 
        } 
        
        get <- function() x 
        
        setInversedMatrix <- function(inverse) inversedMatrix <<- inverse 
        
        getInversedMatrix <- function() inversedMatrix 
        
        list(set=set, get=get, setInversedMatrix=setInversedMatrix, getInversedMatrix=getInversedMatrix) 
} 



## This function computes the inverse of the matrix object created in the first function 
## called makeCacheMatrix.  If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
        
        inversedMatrix <- x$getInversedMatrix() 
        
        if(!is.null(inversedMatrix)) { 
                message("getting cached data.") 
                return(inversedMatrix) 
        } 
        
        data <- x$get() 
        
        # Documentation for Solve can be found here: http://www.statmethods.net/advstats/matrix.html
        # solve computes the inverse of the matrix.
        inversedMatrix <- solve(data) 
        
        x$setInversedMatrix(inversedMatrix) 
        
        inversedMatrix 
}
