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


