## Write a function which creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function (y){
		x <<- y
		i <<- NULL
	}
	get <- function (){
		x
	}
	## Creat a function to change the value of i to calculated inverse
	setInverse <- function(inverse){
		i <<- inverse
	}
	getInverse <- function(){
		i
	}
	## Function "makeCacheMatrix" returns a list including four functions 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	## If the inverse has already been calculated and unchanged, then return the value directly
	if (!is.null(i)){
		message("getting cached data")
		return (i)
	}
	## If not, calculate the inverse using solve()
	data <- x$get()
	i <- solve(data,...)
	x$setInverse(i)
	i
}