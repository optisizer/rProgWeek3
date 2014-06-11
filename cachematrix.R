# Function makeCacheMatrix creates a list,
# holding four separte functions that:
# Creates a matrix (set) in cache memory, displayes the matrix (get),
# creates an inverse of the matrix (setinv) in
# cache memeory, and displays the inverse matrix (getinv).
# However, the functions do nothing when makeCacheMatrix is run.
# They need to be called separately or from the function
# cacheSolve
# When creating the matrix with $set, just type in one
# number (y), designating the number of rows and columns.
# The matrix will be filled with y^2 values, using runif().
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    # Function set:
    set <- function(y)  {
        x <<- matrix(data = runif(y^2), nrow = y, ncol = y)
        s <<- NULL
    }
    # Function get:
    get <- function() x
    # Function setinv:
    setinv <- function(solve) s <<- solve
    # Function getinv:
    getinv <- function() s
    # The list holding the four functions:
    list(set = set, get = get, setinv = setinv, getinv = getinv)    
}

# Function cacheSolve calls the two functions getinv and setinv
# from the list created by makeCacheMatrix.
# The if-statement splits the outcome of the function into two
# branshes, with separate and different outputs.
cacheSolve <- function(x, ...) {
    # Return an inverted matrix from cache memory
    # (empty if a new original matrix has been created):
    s <- x$getinv()
    #If the inverse matrix is NOT empty then:
    if(!is.null(s)) {
        message("getting cached data")
        # Displays the inverse matrix, and exits the function:
        return(s)
    }
    # Therefore: If the inverse matrix IS empty then:
    # Calls the original matrix from cache memory:
    data <- x$get()
    # Creates an inverse matrix:
    s <- solve(data, ...)
    # Passes it to setinv, so that no longer empty:
    x$setinv(s)
    # Display the inverse matrix:
    s
}