#' A rasch
#' 
#' Object of class \code{Rasch} are created by the \code{integrateIt} functions
#'
#' 
#' An object of the class "Trapezoid" or "Simpsons" has the following slots:
#' \itemize{
#' \item \code{name} A character string indicating the name of the student
#' \item \code{a} A vector of difficulty values for each test question
#' \item \code{yJ} A vector off corresponding binary values indicating a correct (1) or incorrect (0) answer from the student
#' }
#'
#' @author Aaron J. Brezel: \email{aaronbrezel@@wustl.edu}
#' @rdname raschClass
#' @export
setClass(Class="Rasch",  #setClass method. creates Student class with three parameters: name, a and yJ
         representation = representation(
           name = "character",
           a = "numeric",
           yJ = "numeric"
         ),
         prototype = prototype(
           name = c(),
           a = c(),
           yJ = c()
         )
)

#' @export
setMethod("initialize", "Rasch", #initialize method
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
)  


#' @export
setValidity("Rasch", function(object){ #Validity Function
  if(!identical(length(object@a), length(object@yJ))){ #a and yJ need to have an identical lengths. That's because their indecies must match up in order to pair the right question difficulty (a) with the corresponding question answer (yJ)
    return("Yo, vectors a and yJ must have identical lengths")
  }
})
