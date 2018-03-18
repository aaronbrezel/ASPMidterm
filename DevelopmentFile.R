## Load libraries, set working directory and create a package skeleton
library(devtools)
library(roxygen2)
setwd("C:/users/aaron/OneDrive/documents/Applied Statistical Programming/ASPMidterm")
package.skeleton()

## code to construct the package
current.code <- as.package("easyRasch")
load_all(current.code)
document(current.code)
check(current.code)


##test math

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

setMethod("initialize", "Rasch", #initialize method
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

setValidity("Rasch", function(object){ #Validity Function
  if(!identical(length(object@a), length(object@yJ))){ #a and yJ need to have an identical lengths. That's because their indecies must match up in order to pair the right question difficulty (a) with the corresponding question answer (yJ)
    return("Yo, vectors a and yJ must have identical lengths")
  }
})

a <- c(1,2,3,4,5,6,7,8,9,10)
yJ <- c(1,0,1,1,0,1,1,1,0,1)
Brian <- new("Rasch", name = "Brian", a = a, yJ = yJ)
Brian
