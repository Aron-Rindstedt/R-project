init <- function() {
  packages <- installed.packages()
  #TODO: Install missing required packages
  #TODO: Include required packages
  
  # Import data file
  if (!exists("shelter.data")){
    cat("Please select the shelter data text file.\n")
    shelter.path <- file.choose()
    shelter.data <- read.table(shelter.path, header=T) # Import poly4 data
    assign("shelter.data", shelter.data, envir = .GlobalEnv)
  }
}

#TODO Examine importance of features (even if you will not discard any).
#TODO If too many features:
  #TODO Select those of low importance from step 2 to remove.
  #TODO Reduce dimensionality using PCA.
#TODO Select a number of model types.
  #TODO Create models of each type, using an appropriate range of hyperparameters and from different features if desired (ie some from PCA, others from natural features)
  #TODO Select best model.
#TODO Evaluate selected model