makeCacheMatrix <- function(x = matrix()) { #make the matrix
      inv <- NULL 
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x #create the function to solve for the inverse
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

cacheSolve <- function(x, ...) { #use the function to solve
      inv <- x$getinv()
      if(!is.null(inv)) { #create the message if the inv is null
            message("getting cached result")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}