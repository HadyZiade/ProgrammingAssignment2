## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		v_inv <- NULL
		set <-function(y) {
			x <<- y
			v_inv <<- NULL
		}
        get <- function() x
        setinv <- function(inv) v_inv <<- inv
        getinv <- function() v_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		v_inv <- x$getinv()
        if(!is.null(v_inv)) {
                message("getting inverse cache")
                return(v_inv)
        }
        data <- x$get()
        v_inv <- solve(data)
        x$setinv(v_inv)
        v_inv
}

