##Daniel Piper Programming assignment Lesson 3 - "R Programming submssion"

#makecachematrix Function
#This function inputs a matrix, sets the value of matrix, gets the value of the matrix
makeCacheMatrix <- function(xm=matrix()){
  mx < -NULL #holds value of matrix inverse
  
  #set value of matrix
  
  set <- function(y){  #assign new matrix
    xm <<- y   #find value of matrix in parent environment
    mx <- NULL #set inverse value to NULL if matrix in parent env exists
    
  }
  
  getmatrix <- function() xm  #get/return value of matrix
  setinverse <- function(inverse) mx <<- inverse  #set inverse value in parent environment
  getinverse <- function() mx  #get inverse value when called
  list(set=set,getmatrix=getmatrix,
       setinverse-setinverse, 
       getinverse=getinverse)
}

##Get value of matrix inverse from the make cache function
cacheSolve <- function(xm,...){
  mx <- xm$getinverse  #find/retrieve invertable matrix
  
  #if inverse matrix in not NULL, return cached inverse
  if(!is.null(mx)){
    message("getting cached data")
    return(mx) #return inverse of matrix
  }
  
  data <- xm$getmatrix() #calling get matrix function to return matrix value
  mx <- inverse(data,...) #compute inverse
  xm$setinverse(mx) #set inverse value into parent environment
  mx #return invertable matrix
  
}