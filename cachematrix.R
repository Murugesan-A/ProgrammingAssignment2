## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrix1 = matrix()){
        
        inv.matrix <- NULL
        
        set <- function(param.matrix){
                matrix1 <<- param.matrix
                inv.matrix <<- NULL
        }
        
        get <- function() matrix1	
        set.inv.matrix <- function( mat )inv.matrix <<- mat	
        get.inv.matrix <- function() inv.matrix	
        list( set = set, get = get, set.inv.matrix = set.inv.matrix, get.inv.matrix = get.inv.matrix)
}

## Write a short comment describing this function

cacheSolve <- function(matrix1, ...){
        
        inv.matrix <- matrix1$get.inv.matrix()
        
        if( !is.null( inv.matrix )){
                message( "Getting cached data..." )
                return( inv.matrix )	 
        }
        
        data <- matrix1$get()
        inv.matrix <- solve(data)
        matrix1$set.inv.matrix(inv.matrix)
        inv.matrix
}
