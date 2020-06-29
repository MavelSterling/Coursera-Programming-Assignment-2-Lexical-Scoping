# Example: Caching the Mean of a Vector
# The first function, makeVector creates a special "vector", 
# which is really a list containing a function to

# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4.get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# The following function calculates the mean of the special "vector" created 
# with the above function. However, it first checks to see 
# if the mean has already been calculated.
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of 
# the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

##### Assignment: Caching the Inverse of a Matrix


makeCacheMatrix <- function(x = matrix()) {
  Inver <- NULL # Inver as NULL
  set <- function(y) { # create the set function
    x <<- y # Looking for definition of x
    Inver <<- NULL # Looking for definition of Inver
  }
  get <- function() x
  setInver <- function() Inver <<- solve(x) # Calculate the inverse
  getInver <- function() Inver
  list(set = set,
       get = get,
       setInver = setInver,
       getInver = getInver)
}

#EXAMPLE:
M <- makeCacheMatrix()
M$set(matrix(c(3,1,2,1),nrow=2,ncol=2)) # The matrix 2x2 is created
M$get() # Data is displayed

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inver <- x$getInver()
  if(!is.null(Inver)){ # Check if Inver is null
    message("Getting cached data") 
    return(Inver)
  }
  Data <- x$get()
  Inver <- solve(Data) # Calculate the inverse of Data
  x$setInver(Inver)
  return(Inver)
}

# EXAMPLE:

Matrix1<- matrix(c(3,1,2,1),nrow=2,ncol=2) # The matrix 2x2 is created

M <- makeCacheMatrix(Matrix1)

cacheSolve(M) # Inverse matrix of Matrix1

