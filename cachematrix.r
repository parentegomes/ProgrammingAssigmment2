#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
	if(!is.matrix(x)|det(x)==0){
		stop
	}

	#set the matrix
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}

	#get the matrix
	get<-function()x

	#set the inverse
	setinverse<-function(inverse) inv<<-inverse

	#get the inverse
	getinverse<-function()inv
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...){

	#verifying if the inverse matrix has already been calculated
	inv<-x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}

	#calculate the inverse matrix if it hasn't been calculated yet
	data<-x$get()
	inv<-solve(data)
	x$setinverse(inv)
	inv
}


