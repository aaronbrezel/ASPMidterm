#' Calculated likelihood of a proposed value of theta
#'
#' Takes an object of class Rasch (which includes the difficulty of each question) and a theta ("skill of the student") value
#' and uses them to calculate the likelihood of a proposed value of theta  
#'
#' @param raschObj An object of class "Rasch." Includes a vector of question difficulty and a corresponding vector indicating whether the student referenced in the object got the question correct (1) or incorrect (0) 
#' @param theta A numeric value indicating the skill of the student taking the test
#'
#' @return The likelihood of a proposed value of theta  
#'  \item{output}{The product sum calculated by multiplying the results of the second output of raschProbability all together}
#' @author Aaron J. Brezel
#' @note This is a very complicated function function
#' @examples
#' 
#' a <- c(1,2,3,4,5,6,7,8,9,10)
#' yJ <- c(1,0,1,1,0,1,1,1,0,1)
#' Brian <- new("Rasch", name = "Brian", a = a, yJ = yJ)
#' theta = 5
#' raschLikelihood(Brian, 5)
#' @seealso \code{\link{raschProbability}}
#' @rdname raschProbability
#' @aliases raschLikelihood,ANY-method
#' @export
setGeneric(name="raschLikelihood",
           def=function(raschObj, theta)
           {standardGeneric("raschLikelihood")}
)

#' @export
setMethod(f="raschLikelihood",
          definition=function(raschObj, theta){
            if(class(raschObj) != "Rasch"){# rashObj must be an object of class Rasch 
              return("Yo, your rashObj must be an object of class Rasch")
            }
            else if(length(theta) != 1){ #checks that the inputed theta is just one number, not a vector or something weird and unexpected
              return("Yo, your theta value must be a single numeric number")
            }
            else if(!is.numeric(theta)){ #checks that the inputed theta is a numeric 
              return("Yo, your theta value must be a single numeric number")
            }
            return(prod(raschProbability(raschObj, theta)[[2]])) #prod method automatically calculates the probability sum. All that's needed is to call the second output of the raschProbability output list
          }
)
