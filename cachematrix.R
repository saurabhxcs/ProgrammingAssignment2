## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
			m <- NULL
			 set<-function(y)	{
				x<<-y
				m<<-NULL
						}
			 get<-function() x			#get x
			 setinv<-function(inv) m<<-inv	#set inverse of x
			 getinv<-function() m			#get inverse of x or value of m
			list(set=set,get=get,setinv=setinv,getinv=getinv)			
}


## Return a matrix that is inverse of 'x'

cacheSolve <- function(x, ...) {
		m<-x$getinv()		#get teh value inverse of x
		if(!is.null(m)){		#if the m is already there in cache return it
			message("getting cached data")
			return(m)
				   }
			data<-x$get()
			m<-solve(data)	#calculate inverse of x
			x$setinv(m)
			m	
}








