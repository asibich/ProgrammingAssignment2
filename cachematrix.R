## Defined below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(original_matrix = matrix()) {
  m_Inverse <- NULL
  set <- function(input_matrix) {
    original_matrix <<- input_matrix
    m_Inverse <<- NULL
  }
  get <- function() original_matrix
  setInverse <- function(input_Inverse) {
    m_Inverse <<- input_Inverse
  }
  getInverse <- function() m_Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function or retrieves the inverse from 
## cache if it has already been calculated.

cacheSolve <- function(cachedInstance, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  computed_inverse <- cachedInstance$getInverse()
  if(!is.null(computed_inverse)) {
    message("getting cached data")
    return(computed_inverse)
  }
  retrieved_matrix <- cachedInstance$get()
  computed_inverse <- solve(retrieved_matrix, ...)
  cachedInstance$setInverse(computed_inverse)
  computed_inverse
}

