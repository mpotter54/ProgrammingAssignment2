## makeCacheMatrix creates and stores cached matrix
## usage a<-cacheMatrix() init a with empty matrix, set cache value to NULL
##      a$set(c(1,2,3,4),ncol=2,nrow=2) set matrix to 2x2 matrix
##      a$get retrieve original marix
##      a$setmtx set cache value a matrix to the inverse
##      a$getmtx get cached inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmtx <- function(mtx) m <<- mtx
        getmtx <- function() m
        list(set = set, get = get,
             setmtx = setmtx,
             getmtx = getmtx)
        
}

## cacheSolve gets a cacheable matrix and returns cache value if in cache otherwise computes and stores the inverse
## usage cacheSolve(a) get the matrix inverse, returning a cached value it it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmtx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmtx(m)
        m
}
