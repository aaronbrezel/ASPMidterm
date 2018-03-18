#' EAP print
#'
#' Prints the EAP value for a certain student 
#'
#' @param raschObj An object of class "Rasch." Includes a vector of question difficulty and a corresponding vector indicating whether the student referenced in the object got the question correct (1) or incorrect (0) 
#'
#' @return The expected a posteriori value for the student and their name  
#'  \item{output} The expected a posterior value and the student's name
#' @author Aaron J. Brezel
#' @note This is a very complicated function function
#' @examples
#' 
#' a <- c(1,2,3,4,5,6,7,8,9,10)
#' yJ <- c(1,0,1,1,0,1,1,1,0,1)
#' Brian <- new("Rasch", name = "Brian", a = a, yJ = yJ)
#' raschPrint(Brian, lower, upper)
#' @seealso \code{\link{raschProbability, raschLikelihood, raschPrior, raschEAP}}
#' @rdname raschPrint
#' @aliases raschPrint,ANY-method
#' @export

setMethod("print", "Rasch", 
          function(x){
            print(x@name)
            print(raschEAP(x, -6, 6))
          }
)