## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Almacenamiento en caché del inverso de una matriz: 
## La inversión de la matriz suele ser un cálculo costoso y puede haber algún beneficio ## para almacenar en caché el inverso de una matriz en lugar de computarlo repetidamente. 
## A continuación hay un par de funciones que se utilizan para crear un objeto especial que 
## almacena una matriz y almacena su inversa.
## Esta función crea un objeto "matriz" especial que puede almacenar en caché su inverso.
## Almacenamiento en caché del inverso de una matriz: 
## La inversión de la matriz suele ser un cálculo costoso y puede haber algún beneficio ## para almacenar en caché el inverso de una matriz en lugar de computarlo repetidamente. 
## A continuación hay un par de funciones que se utilizan para crear un objeto especial que 
## almacena una matriz y almacena su inversa.
## Esta función crea un objeto "matriz" especial que puede almacenar en caché su inverso.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## Esta función calcula la inversa de la "matriz" especial creada por 
## makeCacheMatrix arriba. Si el inverso ya se ha calculado (y la matriz ## no ha cambiado), debería recuperar el inverso de la caché.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
