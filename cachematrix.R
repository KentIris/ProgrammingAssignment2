# The combination of this two functions together, can create a matrix, calculate
# the inverse of the matrix, and store the value of the inverse. Such that, the 
# pair together can save time about re-calculate same inverse of the matrix.

# This makeCacheMatrix function can create a matrix, and save the value of the
# function into a list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y    # If want change the matrix, this stp will reset the
                m <- NULL  # the function and reset "m"
        }
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m      
        list(set = set, get = get,   # Store all the function into a list with
             setinverse = setinverse,# the same name(head name)
             getinverse = getinverse)
}



# This cacheSolve function will calculate the inverse of given matrix and
# save the inverse in makeCacheMatrix function. So that the next, it has 
# no need to re-calculate again and save time. NOTE: if the inverse is called
# from cache, it shows the message.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)){  # if the function has an exist inverse, then it will
                           # return the exist value and quit the function                
                message("getting cached data")
                return(m)
        }       # If no previous inverse, it will start the calculation.
        data <- x$get()
        m <- solve(data, ...)  # Calculate the inverse of the matrix here
        x$setinverse(m)  # set 'm' so that is will not be NULL
        m  # print m 
        ## Return a matrix that is the inverse of 'x'
}
