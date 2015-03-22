## Steps to Run 
        ### mxx <- makeCacheMatrix(matrix(rnorm(25,mean = 3),5,5))
        ### cacheSolve(mxx)
        ### cacheSolve(mxx)


# Description of the function
        ## returns a list of all the functions on the matrix
        ## makeCacheMatrix acts as a Bean(in java ) here
makeCacheMatrix <- function(x = matrix()) {
        inversedMatrix <- NULL
        ## set the matrix x 
        setMatrix <- function(matrix){
                x <<- matrix
                inversedMatrix <<- NULL        
        }
        
        ## get the original matrix x 
        getMatrix <- function() x
        
        ## sets the inverse of the matrix to the global variable using the "<--" operator
        setInverseMatrix <- function(invMatrix){
                inversedMatrix <<- invMatrix
        }
        
        ## get the inverse of the matrix  : returns 
        getInverseMatrix <- function() inversedMatrix
        
        ## returns the list (of functions)
        list(set=setMatrix,get = getMatrix,setInv = setInverseMatrix,getInv = getInverseMatrix)
}


# cacheSolve function provides the same functionality as 'solve(matrix)' with an additional feature of # cache ( inorder to overcome the computation of the same data set again and again)
        
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        invMatrix  <- x$getInv()
        if(!is.null(invMatrix)){
                message("getting the inverse of the matrix from the cache !! ")
                return(invMatrix)
        }
        matrix <- x$get()
        invMatrix <- solve(matrix)
        x$setInv(invMatrix)
        invMatrix
}
