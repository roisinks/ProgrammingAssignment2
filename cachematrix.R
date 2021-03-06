#set the directory of where to save the file to

setwd("~/R/x86_64-pc-linux-gnu-library/3.6/ProgrammingAssignment2-master")

#This function creates a special "matrix" object that can cache its inverse
m <- makeCacheMatrix(x = matrix(c(1,2,6,7), nrow = 4, ncol = 4));
summary(m);

makeCacheMatrix <- function(x = matrix(c(1,2,6,7), nrow = 4, ncol = 4)) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix = set the value of the matrix
  ##              2. get the matrix =  get the value of the matrix
  ##              3. set the inverse = set the cached value (inverse matrix)
  ##              4. get the inverse =  get the cached value (inverse matrix)
  ##         this list is used as the input to cacheSolve()
  #sets to null as nothing can be initially cached as is empty
  inv = NULL
  #store the matrix
  setM = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    #renullifies the cache after assigning a matrix value
    inv <<- NULL
  }
  #return a matrix
  get = function() {
    x
  }
  #inverse given matix
  setSolve = function(inverse){
    inv <- inverse
  }
  #obtained the cached matrix value
  getSolve = function() { 
    inv
  }
  # a list with very element as a function
  list(setM=setM, get=get, setSolve=setSolve, getSolve=getSolve)

}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#make the function 
cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- y$getSolve()
  # ireturns cache value if available
  if(!is.null(inv)) {
    message("getting the inverse matrix")
    return(inv)
  }
  #calculate inverse of matrix and store it
  data <- y$getM()
  inv <- solve(data)
  y$setSolve(inv)
  
  # return the inverse
  inv
}
  

