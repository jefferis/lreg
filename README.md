# lreg

A simple example of a package using S4 classes and roxygen2 for documentation.

See http://stackoverflow.com/questions/25569870/developing-r-package-when-functions-are-written-in-s4-and-using-roxygen2

## Testing

* clone the repository
* start R

```r
setwd("/path/to/lreg")
library(devtools)
check()
```

## Installation
Should you just wish to install this package, you can use **devtools**:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferis/lreg")
```
