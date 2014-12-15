## Well, my function is just imitating the sample of mean calculation, just substituted the class
## "numeric" with "matrix" and the function mean() with solve().

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        InvMtrx<-NULL    ##Initialize InvMtrx = NULL        
        set<-function(y=matrix()) {
                x <<- y
                InvMtrx <<- NULL
        }  
        get <- function() x
        setInvMtrx <- function(Inv) InvMtrx <<- Inv
        getInvMtrx <- function() InvMtrx
        list(set=set, get=get, setInvMtrx=setInvMtrx, getInvMtrx=getInvMtrx)

}


## A function that makes cache of matrix. In fact it returns a list of function: 
## 1. set matrix 2. get matrix 3. set inversed matrix 4. get inversed matrix

cacheSolve <- function(x, ...) {
        InvMtrx <- x$getInvMtrx()
        if (!is.null(InvMtrx)) {
                message("getting cached inversed matrix")
                return(InvMtrx)
        }
        ## To check if the inversed matrix has been calculated. If yes, ruturn it.
        data <- x$get()
        InvMtrx<-solve(data,...)
        InvMtrx
        ## Return a matrix that is the inverse of 'x'
}
