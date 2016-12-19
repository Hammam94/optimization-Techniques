library(rootSolve)

# get the value of the function at specific point
getCurrentValue <- function(c){
  x <- c[1]
  y <- c[2]
  return(eval(x - y + 2 * (x ^ 2) + 2 * x * y + y ^ 2))
}

#test for optimal
testMyPoint <- function(point, anonymous) {
  x <- point[1]
  y <- point[2]
  print(x)
  print(y)
  firstDerivativeFunction <- Deriv(x - y + 2 * (x^2) + 2 * x * y + y^2 , anonymous)
  print(firstDerivativeFunction)
  print(eval(firstDerivativeFunction))
  return(eval(firstDerivativeFunction))
}

getmyListOfSubsituation <- function( currentPoint, searchDirection, directionType ) {
  return(substitute(list(x = a + w * r, y = b + z * r),list(a = currentPoint[1], b = currentPoint[2], w = searchDirection[1], z = searchDirection[2] )))
}

#return optimal point
getMyPoint <- function(startPoint, searchDirection, optimalSteps, type) {
  optimalPoint <- startPoint
  optimalValue <- getCurrentValue(startPoint)
  for( i in 1:length(optimalSteps)){
    currentPoint<- startPoint + optimalSteps[i] * searchDirection
    if(type && optimalValue < getCurrentValue(currentPoint)){
      optimalValue <- getCurrentValue(currentPoint)
      optimalPoint <- currentPoint
    }else if(!type && optimalValue > getCurrentValue(currentPoint)){
      optimalValue <- getCurrentValue(currentPoint)
      optimalPoint <- currentPoint
    }
  }
  return(optimalPoint)
}

getOptimalProbLength <- function(listOfSubsitituations){
  newFunction <- substitute(x - y + 2 * (x^2) + 2 * x * y + y^2, listOfSubsitituations)
  firstDerivativeFunction <- Deriv(newFunction, "r")
  f <- function(r) eval(firstDerivativeFunction)
  return(uniroot.all(f, c(-50,50)))
}

univariateSearchStep <- function(currentPoint, problength, searchDirection, type) {
  #get value of f(x) & f+ & f-
  positivePoint = currentPoint + problength * searchDirection
  negativePoint = currentPoint - problength * searchDirection
  
  currentValue = getCurrentValue(currentPoint)
  positiveValue= getCurrentValue(positivePoint)
  negativeValue= getCurrentValue(negativePoint)
  
  #init current State value
  currentState <- 0
  
  # maximization if true and minization if false
  if( type ) currentState <- max(currentValue, positiveValue, negativeValue)
  else currentState <- min(currentValue, positiveValue, negativeValue)

  #if my current point is alredy the optimal point
  if( currentState == currentValue ) return(currentPoint)
  
  #continue to get the next point
  
  #if f+ is the derication
  if( currentState == positiveValue ) {
    optimalSteps <- getOptimalProbLength(as.list(getmyListOfSubsituation(positivePoint, searchDirection, TRUE )))
    return(getMyPoint(positivePoint, searchDirection, optimalSteps, type))
  }
  #if f- is the derication
  else {
    optimalSteps <- getOptimalProbLength(as.list(getmyListOfSubsituation(negativePoint, -1 * searchDirection, FALSE )))
    return(getMyPoint(negativePoint, -1 * searchDirection, optimalSteps, type))
  }
}

getSearchDirection <- function(iteration, numberOfDirections){
  SearchDirectionVector <- c(rep(0, numberOfDirections))
  index <- iteration %% numberOfDirections
  if(index == 0) index <- numberOfDirections
  SearchDirectionVector[index] <- 1
  return(SearchDirectionVector)
}

univariateSearch <- function(startPoint, probeLength, numberOfIteration, type) {
  myPoint <- startPoint
  myList <- list()
  for(i in 1:numberOfIteration){
    myPoint <- univariateSearchStep(myPoint, probeLength, getSearchDirection(i, length(startPoint)), type)
    myList <- c(myList, list(myPoint))
    testOptimalX <- testMyPoint(myPoint, "x")
    testOptimaly <- testMyPoint(myPoint, "y")
    #if(testOptimal < 0.01 && testOptimal > -0.01) break
  }
  
  for(i in 1:length(myList)){
    print(myList[i])
  }
  lastElement <- myList[length(myList)]
  print("the optimal point is:")
  print(lastElement)

  #getOptimalProbLength(list(x = quote(2*r), y = 0))
}



