## Put comments here that give an overall description of what your
## functions do

## This function takes the argument x=matrix and then asumes that the matrix is invertible. It creates a square matrix and its inverse
##Afterwards the values of the matrix are set 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
}
        get<- function() (x)
                setInverse<-function(inverse)(inv<<-inverse)
                        getInversez<-function()(inv)
                                list(set=set, get=get; setInverse=setInverse, getInverse=getInverse)
                        }
        


## This functions gives the inverse of the matrix that has been returned by the function makeCacheMatrix above
##This is if the inverse has been already calculated and the matrix has not been changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
       }
        mat<-x$get()
        inv<-solve(mat, ...)
        x$setInverse(inv)
        inv
        
}
