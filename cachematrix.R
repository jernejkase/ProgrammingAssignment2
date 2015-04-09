## Functions implement matrix with cachaeable inverse 

## constructs a matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	get <- function() x
	set <- function(y){
		x <<- y
		s <<- NULL
	}
	setSolve <- function(solve) s <<- solve
	getSolve <- function() s
	list(get = get, set = set,
		 setSolve = setSolve,
		 getSolve = getSolve)
}


## checks if the matrix already has solve computed
## and returns cache or computes and caches the solve
cacheSolve <- function(x, ...) {
	s <- x$getSolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	m <- x$get()
	s <- solve(m)
	x$setSolve(s)
	s
}
