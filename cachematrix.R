## makeCacheMatrix and cacheSolve are functions used together to calculate and 
## cache the solution to what the inverse of any particular invertible matrix 
## is. The matrix is an input arg to makeCacheMatrix and later accessed or 
## updated through mat1$get() or mat1$set(new_mat), respectively, where mat1 is 
## an example of the name to which we assign the list first returned by 
## makeCacheMatrix. Though mat1 is only a list of functions, it can be used to 
## access the environment where the original matrix is paired with its variable, 
## x, and where its inverse (or NULL) is paired with its variable, inv. Thus, 
## inv is "cached" in this environment. A quick way to check whether the inverse 
## of the matrix associated with mat1 has already been calculated--and to 
## retrieve it--is by using cacheSolve. Just call it with the argument mat1, and 
## it will output the inverse. If the inverse was not already cached, cacheSolve   
## will calculate it, and afterward the inverse will be cached. cacheSolve 
## accesses or assigns the inverse in the cache using x$getinv() or 
## x$setinv(new_inv), respectively, where x in cacheSolve corresponds to 
## mat1.
## The use of these functions takes advantage of lexical scoping in R, and can 
## spare heavy computation associated with repeatedly solving for the inverse of 
## a constant matrix.

## This function creates a special "matrix" object that can cache the inverse of 
## the input, x, an invertible matrix. The ouput is a list of functions, set, 
## get, setinverse, getinverse.
makeCacheMatrix <- function(x = matrix()) {           
        # initialize inv
        inv <- NULL  
        # you can call the following function later to reset to a new invertibe  
        # matrix and flush inv.
        set <- function(mat) {      
                x <<- mat                  
                inv <<- NULL
        }
        # you can call the following function later to return the invertible 
        # matrix, x.
        get <- function() x
        # you can call the following function later to cache the inverse.
        setinverse <- function(inverse) inv <<- inverse 
        # you can call the following function later to return inv.
        getinverse <- function() inv  
        # assign function handles to the above four functions can be called by
        # referring to the returned output of makeCacheMatrix:
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)     
}


## This function computes the inverse of the special "matrix" object returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
        # retrieve cache and assign to inv
        inv <- x$getinverse()
        # check if cache actually had the inverse already stored and, if so,
        # return it
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        # if not, retrieve the invertible matrix through x
        data <- x$get()
        # compute inverse
        inv <- solve(data)
        # set the inverse in the cache through x
        x$setinverse(inv)
        inv
}
