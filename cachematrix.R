## This function gets/sets the matrix and its inverse variables


makeCacheMatrix <- function(x = matrix()) {



	inv <- matrix()

	#setter for Matrix - initialize the matrix and inverse variables
	set <- function(y){	
		x <<- y
		inv <<- matrix(,nrow(x),ncol(x))
	}

	#getter for Matrix - returns the matrix
	get <- function() x

	#set the inverse of matrix to passed value
	setInverse <- function(mInv) inv <<- mInv

	#get the matrix inverse from prev assignment
	getInverse <- function() inv
	
	list(set = set
	        , get = get
	        , setInverse = setInverse
	        , getInverse = getInverse)
}





## This function checks if inverse of the incoming matrix is already computed
## if so, retrieves it from cache
## and if not, computes it and stores it in cache


cacheSolve <- function(x, ...) {
        
## Return a matrix that is the inverse of 'x'


	#get the initialized matrix 
	thisMatrix <- x$get()

	#get the inverse of the matrix from cache
	matrixInverse <- x$getInverse()
	
	#if there is some non-NA value stored as inverse 
	#return the same
	if(length((matrixInverse[!is.na(matrixInverse)])) != 0){
		message("Same matrix - retrieving from cache")
		return(matrixInverse)
	}
	
	#else, compute inverse and store in cache
	matrixInverse <- solve(thisMatrix)
	x$setInverse(matrixInverse)
	matrixInverse
}
	