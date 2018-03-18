#' Calculate the expected a posteriori value for theta
#'
#' Takes and object of class "Rasch", and uses it to calculate the expected a posteriori value 
#'
#' @param raschObj An object of class "Rasch." Includes a vector of question difficulty and a corresponding vector indicating whether the student referenced in the object got the question correct (1) or incorrect (0) 
#' @param lower A numeric value indicating the lowerbound of the comming integration
#' @param upper A numeric value indicating the upperbound of the comming integration
#'
#' @return The expected a posteriori value for theta  
#'  \item{output}
#' @author Aaron J. Brezel
#' @note This is a very complicated function function
#' @examples
#' 
#' a <- c(1,2,3,4,5,6,7,8,9,10)
#' yJ <- c(1,0,1,1,0,1,1,1,0,1)
#' Brian <- new("Rasch", name = "Brian", a = a, yJ = yJ)
#' lower <- -6
#' upper <- 6
#' raschEAP(Brian, lower, upper)
#' @seealso \code{\link{raschProbability, raschLikelihood, raschPrior}}
#' @rdname raschEAP
#' @aliases raschEAP,ANY-method
#' @export
setGeneric(name="raschEAP",
           def=function(raschObj, lower, upper)
           {standardGeneric("raschEAP")}
)

#' @export

setMethod(f="raschEAP",
          definition=function(raschObj, lower = -6, upper = 6){
            if(class(raschObj) != "Rasch"){# rashObj must be an object of class Rasch 
              return("Yo, your rashObj must be an object of class Rasch")
            }
            else if(length(lower) != 1){ #checks to see that the lowerbound input value is a single number
              return("Yo, your lowerbound value must be a single numeric number")
            }
            else if(length(upper) != 1){ #checks to see that the upperbound input value is a single number
              return("Yo, your upperbound value must be a single numeric number")
            }
            else if(!is.numeric(lower)){ #checks that the lowerbound input is a numeric 
              return("Yo, your lowerbound value must be a single numeric number")
            }
            else if(!is.numeric(upper)){ #checks that the upperbound input is a numeric 
              return("Yo, your upperbound value must be a single numeric number")
            }
            else if(lower > upper){ #checks lowerbound input is less than the upperbound input 
              return("Yo, your lowerbound value must be less than your upperbound input")
            }
            numFunc <- function(theta){ #function for the numerator of the EAP function
              f <- theta*raschLikelihood(raschObj,theta)*raschPrior(theta)
              return(f)
            }
            denomFunc <- function(theta){ #function for the denominator of the EAP function
              g <- raschLikelihood(raschObj,theta)*raschPrior(theta)
              return(g)
            }
            
            print(integrate(numFunc(theta), lower, upper))
            #print(integrate(denomFunc, lower, upper))
            areaUnderCurveNum <- 0
            areaUnderCurveDenom <- 0
            for(i in seq(lower, upper, by = 0.01)){ #calculates the height at each interval and multiplies it by 0.01 
              areaUnderCurveNum <- areaUnderCurveNum + (numFunc(i)*0.1)
              areaUnderCurveDenom <- areaUnderCurveDenom + (denomFunc(i)*0.1)
            } 
            #print(areaUnderCurveNum)
            #print(areaUnderCurveDenom)
            return(areaUnderCurveNum/areaUnderCurveDenom)
            #return(integrate(numFunc, lower, upper)/integrate(denomFunc, lower, upper))
          }
)