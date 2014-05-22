#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix())              
        {
        i<- NULL
        #The matrix is saved using SaveMatrix
        SaveMatrix<-function(y){                 
         x<<- y
                i<<-NULL
        }
        #get() recieves the matrix from SaveMatrix
        get<-function()x                           
        #setInverseCache() sets the inverse of the matrix from 'i' in the cache
        setInverseCache<-function(solve) i<<-solve 
        #getInverseCache() gets the inverse value from the cache
        getInverseCache<-function()i               
             list(SaveMatrix=SaveMatrix,get=get,
             setInverseCache=setInverseCache,
             getInverseCache=getInverseCache
        )
}
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve<-function(x, ...){  
        #gets inverse from cache 
        i<-x$getInverseCache()
        #checks if inverse of matrix is already stored in cache
        if(!is.null(i)){
        #if it is already stored then displays the below message and returns cache value        
                message("getting Cached Data")
                return(i)
        }
        #the matrix value is stored in the variable data if running for the first time ie.cache is empty
        data<-x$get()
        #the inverse of the matrix is calculated using "solve" function and is assigned to i and stored in the cache
        i<-solve(data,...)
        x$setInverseCache(i)
        i
}