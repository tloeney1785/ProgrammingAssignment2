

makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL

    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    get <- function() {
    	## Return the matrix
    	m
    }

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        ## Return the inverse property
        i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix 
## If the inverse has already been calculated 
## then the "cachesolve" should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    ##return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
        
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    x$setInverse(m)

    m
}

