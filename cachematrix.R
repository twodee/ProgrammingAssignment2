#!/usr/bin/env Rscript

# Solving a matrix can be an expensive operation. We provide here a matrix
# wrapper that lazily computes the inverse and retains it for quick subsequent
# retrieval.

# ---------------------------------------------------------------------------- 

# makeCacheMatrix builds a matrix + cache + utility functions for getting and
# setting the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # The inverse starts its life uncalculated.
  inverse <<- NULL

  # The utitity functions are closures that share x and its inverse.
  list(set = function(newMatrix) {
         x <<- newMatrix
         inverse <<- NULL
       },
       get = function() { x },
       setInverse = function(newInverse) { inverse <<- newInverse },
       getInverse = function() { inverse }
  )
}

# ---------------------------------------------------------------------------- 

# cacheSolve returns the wrapped matrix's inverse. If not previously computed,
# it computes the inverse and stores it for future calls.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()

  if (!is.null(inverse)) {
    message("getting cached data")
  } else {
    matrix <- x$get()
    inverse <- x$setInverse(solve(matrix))
  }

  inverse 
}

# ---------------------------------------------------------------------------- 

