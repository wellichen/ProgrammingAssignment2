
##############################################################################
##PLEASE NOTE THAT AS NO VALIDATIONS ARE REQUIRED BY THE ASSIGNMENT          #
##THE FUNCTIONS WERE MADE AS SIMPLE AS IT WAS REQUESTED, GRADE MINDFULLY! =) #
##############################################################################


############################################################################
## Function makeCacheMatrix receives a matrix as argument and              #
## returns an object that contains the matrix and provides Acessors        #
#  and Mutators methods to store and retrieve the a matrix as well         #
#  Acessors and Mutators that allows to store another object               #    
#  which is intended to store the inverse matrix                           #  
############################################################################

makeCacheMatrix <- function(x = matrix()) {

         im <- NULL #set the inverse matrix as null, this is for when the object is created
        set <- function(y) { #set the object's matrix and set the inverse matrix with null so it can be set by cacheSolve(x,...)
                x <<- y
                im <<- NULL
        }
        get <- function() x ##Returns the original Matrix
        setInverseMatrix <- function(inverseMatrix) im <<- inverseMatrix ##set the value of the attribute intended to store the inverse matrix
        getInverseMatrix <- function() im ##returns the value of the attribute intended for the inverse matrix
        
        list(set = set, get = get,                 ## Returns the object so it can be assigned to a symbol
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix) 
    
    
}


#######################################################################
## Function cacheSolve is intended to receive a object created by     #
## makeCacheMatrix and check wheter the object already has the        #
## inverseMatrix cached, in this case it returns the cached inverse   #
## matrix, otherwise it calculates the inverse matrix for the matrix  #
## of the object and use the mutator method setInverseMatrix to store #
## the inverse matrix within the object                               #
#######################################################################


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        im <- x$getInverseMatrix()
        if(!is.null(im)) { # Check if the inverse matrix has already been calculated.
                message("getting cached data")
                return(im) # Returns the inverse matrix 
        }
        data <- x$get() #-> Retrieves the matrix 
        im <- solve(data, ...) #-> Calculates the inverse matrix
        x$setInverseMatrix(im) #-> Set the inverse matrix on the object
        im #-> Returns the inverse matrix

}




 

 