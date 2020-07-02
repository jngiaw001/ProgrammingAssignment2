## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that
## we will not discuss here). Your assignment is to write a pair of functions that cache the inverse 
## of a matrix.

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<-function(x=matrix()){          #initialize x as the makeCacheMatrix() argument
            inv<-NULL               #set inv to NULL to initialize it as an object within the makeCacheMatrix() environment
            set<-function(y){       #set() function takes on y as an argument
                        x<<-y       #assign y to x in the parent environment
                        inv<<-NULL    #clears previous inverse matrix if any
            }
            get<-function() x       #defines the getter for the inputted matrix x
            setInverse<-function(solve) inv<<-solve      #defines the setter for the computed inverse (inv)
            getInverse<-function() inv          #defines the getter for the inverse (inv)
            list(set=set,                      #consolidate and name the setters and getters into a list
                 get=get, 
                 setInverse=setInverse, 
                 getInverse=getInverse)
}

## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
            inv<-x$getInverse()         #attempts to retrive the inverse from the matrix inputted
            if(!is.null(inv)){          #if inverse has already been calculated, return the inverse value immediately
                        message("getting cached data")
                        return(inv)
            }
            data<-x$get()         #if inverse value not already calculated, get the matrix
            i<-solve(data, ...)   #calculate the inverse 
            x$setInverse(i)       #set value of inverse
            i                     #return inverse
}
