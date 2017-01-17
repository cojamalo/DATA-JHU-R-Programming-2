## Put comments here that give an overall description of what your
## functions do
## Submission by Connor Lenio. Email: cojamalo@gmail.com
## The following functions create a special matrix object that serves as a cache for a matrix and its inverse. The functions manage both the storage and the retrieval of matrix data from the special matrix object once the special object is created as well as the calculation of the inverse matrix.

## Note: For the functions to work as expected, first initalize an instance of the special matrix object woth makeChaceMatrix by storing the function to a variable, such as my_cache_matrix <- makeCacheMatrix().
## Then, set a sample matrix in makeCacheMatrix or use your own previously creatd matrix by calling the makeCacheMatrix variable name you just used and with its set() method, such as my_cache_matrix$set(matrix(rnorm(9), 3, 3)).
## Now you can call cacheSolve with the makeCacheMatrix object variable as a parameter to find the inverse, such as cacheSolve(my_cache_matrix). 
## Following the steps as above will return the correct inverse matrix as can be checked by using %*% to multiply the original matrix and the inverse by each other to find the expected identity matrix.

## Write a short comment describing this function
## The makeCacheMatrix function initalizes a special "matrix" object in the current environment that can serve as a cache for its own inverse. Specifically, the object contains functions for getting and setting the value of the input matrix as well as its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The cacheSolve function takes an input special matrix object created by makeCacheMatrix and updates the inverse of the matrix set in the makeCacheMatrix object by either setting a newly solved inverse matrix using the matrix data stored in the special object or by recalling the already-solved inverse matrix data in the special object, if the data already exists. 

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
