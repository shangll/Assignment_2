# Assignment_2
# name the function which consists of 2 variables: x & s
# and x is matrix
makeCacheMatri <- function(x = matirx()) {
  # s is used to store the result
  s <- NULL
  # assign value to x that belongs to makeCacheMatri
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  # return x which is the data need be processed
  get <- function() x
  # assign the value of mean to s
  setsolve <- function(solve) s <<- solve
  # return s which is the result of processing
  getsolve <- function() s
  # makeCacheMatri returns a list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
} 

# x in this function must be makeCacheMatri
cacheSolve <- function(x, ...) {
  # use getsolve which belongs to makeCacheMatri
  # searching whether has calculated inverse of matrix or not
  # then function will return makeCacheMatri
  s <- x$getsolve()
  # if inverse of matrix is not NULL
  # it means that it has been caculated before
  # then return the result
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  # in this condition, the inverse of matrix has not been calculated
  # use the get() which belongs to x
  data <- x$get()
  # calculate the inverse of matrix
  s <- solve(data, ...)
  # use the setsolve() which belongs to x
  # return the result to x
  # then use cacheSolve next time
  # the result will be store by x
  # which means it need not be calculated
  # cacheSolve will return in the step of searching
  x$setsolve(s)
  # return the result
  s
}
