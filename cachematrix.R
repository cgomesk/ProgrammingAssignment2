# PROGRAMMING ASSIGNMENT 2 (Dec2018)

## This function creates a special "matrix" object that can cache the inverse of a matrix
## to be available if needed for further computation.

## The first part of the function aims at creating the special object (matrix), which is a list containing four functions.


makeCacheMatrix <- function(x = matrix()) { 
  # create the function 'makeCacheMatrix' with argument x (= object type matrix)
              m <- NULL 
              # create an empty object "m" to hold the invert matrix
              set <- function (y){
                  x <<- y # set y = x, where y is assumed to be a matrix (the double '<<-' means assignment outside of the current environment)
                  m <<- NULL # set NULL value to m
              }
              # function 1 = create function 'set' with argument y (x in another environment) 
              get <- function ()x
              # function 2 = create function 'get' to retrieved "x" from the parent environment (as it is not included in the 'get' function)
              setsolve <- function(solve) m <<- solve
              # function 3 = create function 'setsolve' to get "m" from parent environment and compute inverted matrix
              getsolve <- function()m
              # function 4 = create function 'getsolve' to retrieved "m" from the parent environment
              list (set = set, get = get,
                    setsolve = setsolve, getsolve = getsolve)
              # assign each of the four functions as elements of a list and name the elements (elementName = value);
              # naming the list elements allows us to use the '$' extract operator to get the contents of the matrix in subsequent code.
}

## The second part of the function aims at calculating the inverse matrix of the special object (matrix) created above,
## BUT it checks first if this calculation has already been performed earlier and is available in cache.

cacheSolve <- function(x, ...) {
  # create the function 'cacheSolve' with argument x plus additional arguments (represented by the ellipsis = three dots)
          m <- x$getsolve()
          # extract the elements of 'getsolve' from x and assign it to m
          if (!is.null(m)){
          # check if m is not NULL
                message("getting cached data")
                # if not NULL, show message between "XXX", which means that there is a valid matrix cached 
                return (m)
                # and it can be returned to parent environment without having to be computed (again)
          }
          data <- x$get()
          # if m is NULL (there is no cached matrix), get the matrix of x and assign it to data
          m <- solve (data, ...)
          # calculate the inverse matrix of data and assign it to m
          x$setsolve(m)
          # store inverse matrix of x to cache, so it is not necessary to be calculated again
          m
          # print m (the inverse matrix of x)
          
}
## Return a matrix (m) that is the inverse of 'x'

## To test function:

#x <- matrix (1:4, nrow=2, ncol=2)
#cacheSolve (makeCacheMatrix(x))
