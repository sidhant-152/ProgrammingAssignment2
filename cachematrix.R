## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x=matrix()){
    inv <- NULL
	##set function
    set <- function(y){
        x <<-y
        inv <<-NULL
    }
	##get function
    get <-function()x
	##setInverse function
    setInverse <- function(Inverse)inv <<- Inverse
	##getInverse function
    getInverse <- function()inv
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    ##Inverse of matrix
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Cached Data...........")
        return(inv)
    }
    mat <- x$get()
	
	##using solve() function to get inverse of matrix
    inv <- solve(mat)
    x$setInverse(inv)
    print(inv)
}
