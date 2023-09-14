## Programming Assignment 2: Lexical Scoping
## The functions below do the following:
## Use lexical scoping to perform functions faster by creating a cache
## so we can call the a value when we need it again. We also use the
## <<- operator to assign a value to an object in an enviroment
## different from the current environment.
## Both of the functions below will create an object that stores a matrix
## and then cache's the inverse of that matrix.

## This function creates a list that contains the function to:
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix created from the
## makeCacheMatrix function. First, it checks to see if the inverse has already
## been calculated. If so, it returns the inverse matrix (it doesn't have to
## calculate it again.) If not, it will calculate the inverse of the matrix
## using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
