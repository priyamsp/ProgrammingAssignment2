#Create a function makeCache matrix which creates a special type of list that stores 
#matrix inversed data
makeCacheMatrix <- function(x = matrix()) #x is the input matrix to be inversed
{
  m <- NULL # m is used to store the marix inverse result
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() 
  {
    x
  }
  
  setmatrix <- function(solve) 
  {
    m <<- solve
  }
    
  getmatrix <- function() 
  {
    m
  }
  
  list(set = set, get = get,setmatrix = setmatrix,getmatrix = getmatrix)
}

cacheSolve <- function(x, ...) 
{
  m <- x$getmatrix()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)# to solve for the inverse of matrix (data) and store the result in m
  x$setmatrix(m)
  m
}
