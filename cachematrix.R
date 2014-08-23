##
## These functions will inverse a matrix.  If the matrix inverse has already been cached it
## will notify that it has been cached and list the inversed matrix

## This function creates a special matrix.  
##      1. Set the value of the matrix
##      2. Get the value of the matrix
##      3. Set the value of the inverse
##      4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function creates the inverse of the matric created in the above function.  
## Before it creates the inverse it checks to see if it has already been calculated.
## if it has been it get the results from the cache and does not calculate the result.
## If it does not already have the inverse calculated and sets the value in the cache. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m        
}
