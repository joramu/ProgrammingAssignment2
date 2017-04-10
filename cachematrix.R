##This function creates a special "matrix" object that can cache its inverse
##which is really a list containing a function to
#1. Set the value of the matrix
#2.Get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inverseMatrix<- NULL
        set <- function (y) {
                x<<-y
                inverseMatrix<<-null
        }
        get<- function() x
        setInverseMatrix<-function(inverse) inverseMatrix<<-inverse
        getInverseMatrix<-function()inverseMatrix
        list(set =set, get=get,
             setInverseMatrix=setInverseMatrix,
             getInverseMatrix=getInverseMatrix)
        

}


##The function computes the inverse of special "matrix" returned by makecacheMatrix Above. if The inverse has already been 
## calculated (and the maxtrix has not been changed), then the cachesolved should retrieve the inverse from the cache.
##This function assumes thas the matrix is always invertible
cacheSolve <- function(x, ...) {
        inverseMatrix<-x$getInverseMatrix()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        matrixData<-x$get()
        inverseMatrix<-solve(matrixData,...)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}
