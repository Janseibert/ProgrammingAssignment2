## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse 
## so that when we need it again, it can be looked up in the cache rather than recomputed. This is doen by the following 
## code

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing 
## a function to set the value of the matrix, get the value of the matrix, set the values of the inverse, 
## get the values of the inverse


makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

