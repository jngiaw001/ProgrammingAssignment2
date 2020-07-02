## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix()){
            inv<-NULL
            set<-function(y){
                        x<<-y
                        i<<-NULL
            }
            get<-function() x
            setInverse<-function(solve) inv<<-solve
            getInverse<-function() inv
            list(set=set, 
                 get=get, 
                 setInverse=setInverse, 
                 getInverse=getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv<-x$getInverse()
            if(!is.null(inv)){          #if inverse value already calculated, return value immediately
                        message("getting cached data")
                        return(inv)
            }
            data<-x$get()         #if inverse value not already calculated, get the matrix
            i<-solve(data, ...)   #calculate the inverse
            x$setInverse(i)       #set value of inverse
            i                     #return inverse
}
