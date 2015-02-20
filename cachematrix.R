## makeCacheMatrix holds few functions and their environments
## the priamry purpose of makeCacheMatrix function is to cache
## inverse of a matrix

## cacheSolve - first checks whether the matrix is avaible in cache
## if not, creates the inverse of a matrix and stores/caches it

# further reading
# http://shakthydoss.com/lexical-scoping-rule-r/
# https://darrenjw.wordpress.com/2011/11/23/lexical-scope-and-function-closures-in-r/
# https://github.com/jtleek/modules/blob/master/02_RProgramming/Scoping/index.md
# http://adv-r.had.co.nz/Functions.html

### sample use
# creates 2 x 2 matrix
# a <- matrix(sample(10, 4, replace = TRUE), nrow = 2, ncol = 2)
## OR a <- matrix(c(8,10,4,8), nrow = 2, ncol = 2)
# mcm <- makeCacheMatrix(a)
# ainv <- cacheSolve(mcm) 
# or chain two functions
# ainv <- cacheSolve(makeCacheMatrix(a)) 
# a %*% ainv - prints identity matrix


## this function is a closure. 
# it holds set, get, setmatrix and getmatrix functions,
# their environments and caches the matrix in variable m

makeCacheMatrix <- function(x = matrix()) {
     # set m to NULL
     m <- NULL

     #print("outside inner functions...")
     #print(x)
     #print("done printing x...")

     # sets the value of x to passed matrix i.e. y
     # never gets called
     set <- function(y) {
          # help("<<-") or ?assignOps
          #The operators <<- is normally only used in functions, 
          # and cause a search to made through parent environments 
          # for an existing definition of the variable being assigned.
          # never gets printed
          print("Executing set function")
          print(x)
          print(y)
          print(m)
          x <<- y
          m <<- NULL
     }

     # returns x
     # which was passed to makeCacheMatrix as an argument
     get <- function() {
          print("Executing get function")
          print(x)
          x
     }

     # sets the matrix into m variable
     # m is defined in the parent enviornment of 
     # setmatrix function
     # i.e. m is a free variable and it will be searched 
     # in its parent environment, until it is found
     setmatrix <- function(matrix) {
          print("Executing setmatrix function")
          print(matrix)
          m <<- matrix
     }

     # returns the matrix
     # m is defined in the parent enviornment of 
     # getmatrix function
     # i.e. m is a free variable and it will be searched 
     # in its parent environment, until it is found
     getmatrix <- function() {
          print("Executing getmatrix function")
          print(m)
          m
     }

     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## this function - first checks whether the inverse of 
# a matrix is available in cache. If not, it creates the
# inverse of a matrix and stores it in cache.
# if the inverse of a matrix is available in cache,
# it returns it.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getmatrix()

     # check whether m exists in cache
     # if true, return cached matrix
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }

     # calls get method from makeCacheMatrix
     # which returns matrix passed to makeCacheMatrix
     # as a formal argument and assigns value to data
     data <- x$get()

     #creates inverse of a matrix
     m <- solve(data, ...)

     # calls setmatrix method of makeCacheMatrix
     # passes the inverse of a matrix to makeCacheMatrix
     # makeCacheMatrix stores this inverse matrix in cache
     # variable m
     x$setmatrix(m)

     #print("printing Identity matrix")
     #print(data %*% m)

     # return inverse matrix
     m
}
