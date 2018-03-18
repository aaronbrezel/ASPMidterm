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


##test code

a <- c(1,2,3,4,5,6,7,8,9,10)
yJ <- c(1,0,1,1,0,1,1,1,0,1)
Brian <- new("Rasch", name = "Brian", a = a, yJ = yJ)
Brian

theta <- 5
length(theta)
!is.numeric(theta)
