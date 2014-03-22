# Author: Ingo Feinerer

stemCompletion <-
function(x, dictionary,
         type = c("prevalent", "first", "longest",
                  "none", "random", "shortest"))
    UseMethod("stemCompletion", x)
stemCompletion.PlainTextDocument <-
function(x, dictionary,
         type = c("prevalent", "first", "longest",
                  "none", "random", "shortest"))
{
    tokens <- scan_tokenizer(content(x))
    content(x) <- paste(stemCompletion.character(tokens, dictionary, type),
                        collapse = " ")
    x
}
stemCompletion.character <-
function(x, dictionary,
         type = c("prevalent", "first", "longest",
                  "none", "random", "shortest"))
{
    # TODO: Use words() instead of manual splitting
    if (inherits(dictionary, "Corpus"))
        dictionary <- unlist(lapply(dictionary,
          function(x) strsplit(content(x), "[^[:alnum:]]+")))

    type <- match.arg(type)
    possibleCompletions <- lapply(x, function(w) grep(sprintf("^%s", w),
                                                      dictionary,
                                                      value = TRUE))
    switch(type,
           first = {
               setNames(sapply(possibleCompletions, "[", 1), x)
           },
           longest = {
               ordering <-
                   lapply(possibleCompletions,
                          function(x) order(nchar(x),decreasing = TRUE))
               possibleCompletions <-
                   mapply(function(x, id) x[id], possibleCompletions,
                          ordering, SIMPLIFY = FALSE)
               setNames(sapply(possibleCompletions, "[", 1), x)
           },
           none = {
               setNames(x, x)
           },
           prevalent = {
               possibleCompletions <-
                   lapply(possibleCompletions,
                          function(x) sort(table(x), decreasing = TRUE))
               n <- names(sapply(possibleCompletions, "[", 1))
               setNames(if (length(n)) n else NA, x)
           },
           random = {
               setNames(sapply(possibleCompletions, function(x) {
                   if (length(x)) sample(x, 1) else NA
               }), x)
           },
           shortest = {
               ordering <- lapply(possibleCompletions, function(x) order(nchar(x)))
               possibleCompletions <-
                   mapply(function(x, id) x[id], possibleCompletions,
                          ordering, SIMPLIFY = FALSE)
               setNames(sapply(possibleCompletions, "[", 1), x)
           }
           )
}
