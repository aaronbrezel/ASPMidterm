#' Calculate the height of a normal curve at the specified theta value
#'
#' Takes a theta ("skill of the student") value and uses to calculate the height of the normal distribution curve at that value  
#'
#' @param theta A numeric value indicating the skill of the student taking the test
#'
#' @return  
#'  \item{output}{The height of a normal curve with a mean of 0 and a standard deviation of 3 at the theta (student skill) value}
#' @author Aaron J. Brezel
#' @note This is a very complicated function function
#' @examples
#'
#' theta = 5
#' raschPrior(theta)
#' @rdname raschPrior
#' @aliases raschPrior,ANY-method
#' @export
setGeneric(name="raschPrior",
           def=function(theta)
            {standardGeneric("raschPrior")}
)

#'@export
setMethod(f="raschPrior",
          definition=function(theta){
            return(dnorm(theta, mean = 0, sd = 3))  
          }
)