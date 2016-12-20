library(Deriv)
newton <- function(currentPoint, myFunction, N) {
  x <- currentPoint
  for (i in 1:N) {
    point = currentPoint -(eval(Deriv(myFunction, "x")) / eval(Deriv(Deriv(myFunction, "x"), "x")))
    currentPoint <- point; x<-point
    if(eval(Deriv(myFunction, "x")) >= -.001 && eval(Deriv(myFunction, "x")) <= .001) break 
  }
  cat("the optimal point : ", point, "\n")
}