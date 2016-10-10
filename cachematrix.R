#We use lexical scoping in R to reduce repeated computation, I have written 2 function which help check if the inverse has already been computed, if yes it retrives the value from th eglobal environment else it computes the value.


#makeCacheMatrix creates a matrix and contains all the functions to get and set values.
#Functions : 
#set : sets the value of the matrix
#get : gets the value of the matrix
#Setinv : Sets matrix inverse
#Getinv : Gets the matrix inverse

#We first pass a matrix(invertible square matrix)as arguement to function as input.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # x and m are set in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        #returns the function we have created as a list to the              parent environment with names.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
#This function requires argument that is the output of the above function as an input to the function.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

#Testing the retrival of the function:

a = matrix(c(1,-1,1,2),2,2,T)
#       [,1] [,2]
#[1,]    1   -1
#[2,]    1    2

solve(a)
#       [,1]      [,2]
#[1,]  0.6666667 0.3333333
#[2,] -0.3333333 0.3333333

#using lexical scoping to find inverse of a

mymatrix = makeCacheMatrix(a)

cacheSolve(mymatrix)
#        [,1]      [,2]
#[1,]  0.6666667 0.3333333
#[2,] -0.3333333 0.3333333

#If I run the function again, R retrives the vlue already computed from the global env and doesnt recompute. We know taht because when the command is run again, we can see that if prints out the True condition of the If statement and returns the value of M, that was already computed.

#getting cached data
#       [,1]       [,2]
#[1,] 0.6666667 -0.3333333
#[2,] 0.3333333  0.3333333
