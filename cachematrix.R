##yuwang881's ProgrammingAssignment2

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly 

## this function creates a objetc which can store the inverse result, and reuse it later

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<-y
		inverse <<- NULL
	}
	
	get <- function() x
        setInverse <- function(inverseResult) inverse <<- inverseResult
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## this function uses makeCacheMatrix function to implement cache behavior fot the Matrix inversion
## but it can only cache one result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("cached hit!")
                return(inverse)
        }
        mymatrix <- x$get()
        inverse <- solve(mymatrix, ...)
        x$setInverse(inverse)
        inverse
}
