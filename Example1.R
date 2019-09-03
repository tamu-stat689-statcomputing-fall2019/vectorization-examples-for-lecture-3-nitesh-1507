# Squaring elements of a given vector

square_for <- function(x){
  # [ToDo] Use the for loop
  square = rep(0,length(x))
  for(i in 1:length(x))
  {
    square[i] = x[i] * x[i]
  }
  
  # returning the vector after squaring
  return(square)
}

square_sapply <- function(x){
  # [ToDo] Use the sapply function
  square = sapply(x, function(y) y^2)
  
  # returning the vector after squaring
  return(square)
}

square_vec <- function(x){
  # [ToDo] Use power(^) function in vector form
  square = x^2
  
  # returning the vector after squaring
  return(square)
}

square_vec2 <- function(x){
  # [ToDo] Use multiplication(*) function in vector form
  square = x * x
  
  # returning the vector after squaring
  return(square)
}

# [ToDo] Create a vector x of size 100,000 of normal variables

x <- rnorm(100000)

# [ToDo] Verify that all functions return the same output
Square1 = square_for(x)
Square2 = square_sapply(x)
Square3 = square_vec(x)
Square4 = square_vec2(x)

identical(Square1,Square2)
identical(Square1,Square3)
identical(Square1,Square4)

# [ToDo] Use microbenchmark package to compare three functions in terms of speed
library(microbenchmark)

