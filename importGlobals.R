# summary of undefined global functions provided by CRAN checks
txt <- "aggregate axis complete.cases density dev.flush graphics.off mtext
par plot points rainbow rnorm runif segments var write.table"

# Function provided by Kurt Hornik (July 2, 2015)
imports_for_undefined_globals <-
     function(txt, lst, selective = TRUE)
     {
          if(!missing(txt))
               lst <- scan(what = character(), text = txt, quiet = TRUE)
          nms <- lapply(lst, find)
          ind <- sapply(nms, length) > 0L
          imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
          if(selective) {
               sprintf("importFrom(%s)",
                       vapply(Map(c, names(imp), imp),
                              function(e)
                                   paste0("\"", e, "\"", collapse = ", "),
                              ""))
          } else {
               sprintf("import(\"%s\")", names(imp))
          }
     }                       

# these lines should be added to the namespace
writeLines(imports_for_undefined_globals(txt))
