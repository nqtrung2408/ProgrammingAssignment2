## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x)
{
	inversedMatrix <- NULL
	SetValue <- function(y)
	{
		x <<- y
		inversedMatrix <<- NULL
	}	
	GetValue <- function() x
	SetInverse <- function(m) inversedMatrix <<- m	
	GetInverse <- function() inversedMatrix
	
	list(SetValue = SetValue, GetValue = GetValue, SetInverse = SetInverse, GetInverse = GetInverse)
}

cacheSolve <- function(x,...)
{
	m <- x$GetInverse()
	if (!is.null(m))
	{
		message("getting cached data")
		return (m)
	}
	data <- x$GetValue()
	m <- solve(data)
	x$SetInverse(m)
	m
