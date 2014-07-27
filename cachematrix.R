# These functions save in cache memory the inverse of a matrix (assuming it is
# invertible). For this, the following pair of functions is created.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setmean<-function(x) m<<-solve(x,diag(dim(x)[1]))
	getmean<-function() m
	list(set=set, get=get, setmean=setmean, getmean=getmean)

}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- x$setmean(data)
    x$setmean(m)
    m
}
