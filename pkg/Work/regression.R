## Author: Ingo Feinerer
## Regression tests

library("tm")
data("acq")

# Lazy transformations
acq <- tm_map(acq, removeWords, stopwords("english"), lazy = TRUE)
acq <- tm_map(acq, stemDocument, lazy = TRUE)
acq$lazy
acq[[1]]
acq[[17]]
acq$lazy
content(acq)
acq$lazy
