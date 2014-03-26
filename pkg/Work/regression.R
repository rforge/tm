## Author: Ingo Feinerer
## Regression tests

library("tm")
data("acq")

# Lazy transformations
acq <- tm_map(acq, stemDocument, lazy = TRUE)
meta(acq, type = "corpus")
acq[[1]]
acq[[17]]
meta(acq, type = "corpus")
