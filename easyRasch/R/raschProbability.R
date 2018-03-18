#' Calculated probability of answering the question correctly
#'
#' Takes an object of class Rasch (which includes the difficulty of each question) and a theta ("skill of the student") value
#' and uses them to calculate the probability of answering a question correctly. 
#'
#' @param raschObj An object of class "Rasch." Includes a vector of question difficulty and a corresponding vector indicating whether the student referenced in the object got the question correct (1) or incorrect (0) 
#' @param theta A numeric value of corresponding y values with the same dimensionality as \code{x}.
#'
#' @return An output list. The first slot contains the probability of the student answering each question correctly given the question's difficulty and their skill level (theta). The second slot contains a similar list. Instead of only being the probability of getting a question right, in the slots where the student got the question wrong, the probability of correct value is replaced with the probability of getting the question wrong.  
#'  \item{output}{The output list}
#' @author Aaron J. Brezel
#' @note This is a very complicated function function
#' @examples
#' 
#' a <- c(1,2,3,4,5,6,7,8,9,10)
#' yJ <- c(1,0,1,1,0,1,1,1,0,1)
#' Brian <- new("Rasch", name = "Brian", a = a, yJ = yJ)
#' theta = 5
#' raschProbability(Brian, theta)
#' @seealso \code{\link{raschLikelihood}}
#' @rdname raschProbability
#' @aliases raschProbability,ANY-method
#' @export
setGeneric(name="raschProbability",
           def=function(raschObj, theta)
            {standardGeneric("raschProbability")}
)

#' @export

setMethod(f="raschProbability",
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
            probCorr <- lapply(raschObj@a, function(a){ #creates a list of Pij values 
              temp <- (exp(theta - a))/(1 + exp(theta - a))
              return(temp)
            })
            probCorr <- unlist(probCorr, use.names = FALSE) #turns the list created by lapply into a vector
            which(raschObj@yJ == 0) #figures out which index numbers (questions) the student got wrong. This list of indicies can also be applied to probCorr
            PQ <- probCorr 
            PQ[which(raschObj@yJ == 0)] <- 1 - PQ[which(raschObj@yJ == 0)] #swaps out the Pij values for Qij values when y == 0
            output <- list("probCorr" = probCorr, "PQ" = PQ) #puts both the outputs together as a list for convinence
            return(output)
          }
)