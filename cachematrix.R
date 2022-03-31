## Since matrix inversion can be a costly computation, we may want to cache the 
## inverse of a matrix rather than compute it repeatedly. Below are a pair of 
## functions that cache the inverse of a matrix.

## The function makeCacheMatrix() creates a special "matrix" object that can 
## cache its inverse using "setters" and "getters".

makeCacheMatrix <- function(x = matrix()) {         ## Initialize x; the default value of x is an empty matrix.
      inv <- NULL                                   ## Initialize inv; set to NULL to begin.
      set <- function(y) {                          ## Create a function, set(), with input y (or any name other than x)
            x <<- y                                 ## Assign the input, y, to the object x in the parent environment
            inv <<- NULL                            ## Clears any value of inv that may have been set by a prior execution of cacheSolve()
      }
      get <- function() x                           ## Create a function, get(), which retrieves x from the parent environment 
      setinv <- function(inverse) inv <<- inverse   ## Create a function, setinv(), which assigns input "inverse" to the object inv in the parent environment
      getinv <- function() inv                      ## Create a function, getinv(), which retrieves inv from the parent environment 
      list(set = set,                               ## Assign each function as an element of a list which is returned to the parent environment
           get = get,                               ## Note that each element in this list is named, so we can use the $ operator later
           setinv = setinv,                         
           getinv = getinv)
}

## The function cacheSolve() computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {              ## This function, cacheSolve(), will require an input of type makeCacheMatrix()
      inv <- x$getinv()                       ## Assign the value of getinv to the variable inv (will be NULL or a cached value)
      if(!is.null(inv)) {                     ## If inv is not NULL (that is, if it contains a cached inverse), then...
            message("getting cached data")    ## display this message, and
            return(inv)                       ## return the value of inv (the previously cached inverse)
      }                                       ## Otherwise...
      data <- x$get()                         ## Assign the matrix to a variable, data
      inv <- solve(data, ...)                 ## Use the function solve() to compute the inverse of the matrix (data); we are assuming it is square/invertible
      x$setinv(inv)                           ## Use setinv() to cache the inverse 
      inv                                     ## Return inv to the parent environment / print it.
}
