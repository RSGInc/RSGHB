############################################
#
# Unit Tests for RSGHB
# Author: Jeff Keller (August 2014)
#
############################################

### Directory containing the RSGHB package files
package.dir <- choose.dir(caption = "Select folder containing RSGHB package files")
cat("Package contents located at", package.dir)

### Folders containing the example models
examples.dir <- paste(package.dir, "RSGHB_Examples", sep = "/")
examples <- list.dirs(path = examples.dir, full.names = FALSE, recursive = FALSE)

examples = examples[-1]

### Create a temporary folder to store the unit tests
test.dir <- paste(examples.dir, "temp", sep = "/")
dir.create(test.dir)

results <- list() # results of this most recent version of RSGHB
known   <- list() # results from the last version of RSGHB

### Run the example models
for (example in examples) {
  
  # Print some information
  cat("Running Model: ", example, "\n\nThis may take a few minutes...\n\n")
  setwd(paste(examples.dir, example, sep = "/"))
  
  # Copy known results to the temp folder
  flist <- list.files()
  test.subdir <- paste(test.dir, example, sep = "/")
  dir.create(test.subdir)
  file.copy(flist, test.subdir)
  
  # Load the known RData file (should be the only one)
  setwd(test.subdir)
  load(list.files(pattern = "RData$"))
  model$duration <- NULL # times and duration will almost certainly be different
  model$starttime <- NULL
  model$endtime <- NULL
  known[[example]] <- model
  model <- NULL
  
  # Load the R file that runs the example (should be the only one)
  exRfile <- list.files(pattern = "R$")  
  source(exRfile)
  
  # Store the new results for later comparison
  model$duration <- NULL # times and duration will almost certainly be different
  model$starttime <- NULL
  model$endtime <- NULL
  results[[example]] <- model
  
}

### Compare these new models to the known models
matches <- list()
for (example in names(results)) {
     matches[[example]] <- identical(results[[example]], known[[example]])
     cat("Model Comparison:", example, "   Matches?", if (matches[[example]]) {"Yes"} else {"No"}, "\n\n")
}

### Remove temp folder if everything is all set
if (all(unlist(matches))) {
     setwd(examples.dir)
     unlink(test.dir, recursive = TRUE)
}