run <- function(){
    init()
    coef <- examine()
    shelter.reduced.data <- reduce(coef)
    
    shelter.list <- divide(shelter.reduced.data)
    assign("shelter.train", shelter.list[[1]], envir = .GlobalEnv)
    assign("shelter.val", shelter.list[[2]], envir = .GlobalEnv)
    assign("shelter.test", shelter.list[[3]], envir = .GlobalEnv)

    model <- model()
    return(evaluate(model))
}

divide <- function(x){
    len = length(x[,1])
    t1 = sample(1:len,len*3/5)
    t2 = sample((1:len)[-t1],len/5)
    t3 = (1:len)[-c(t1,t2)]
    
    train = x[t1,]
    val = x[t2,]
    test = x[t3,]
    return(list(train,val,test))
}

init <- function() {
    packages <- installed.packages()
    ##TODO: Install missing required packages
    ##TODO: Include required packages
    
    ## Import data file
    if (!exists("shelter.data")){
        cat("Please select the shelter data text file.\n")
        shelter.path <- file.choose()
        shelter.data <- read.table(shelter.path, header=T)
        assign("shelter.data", shelter.data, envir = .GlobalEnv)
    }
}

##TODO Examine importance of features (even if you will not discard any).
examine <- function(){
    return(summary(
               lm(Items~.,shelter.data))$coefficients)
}

##TODO If too many features:
  ##TODO Select those of low importance from step 2 to remove.
  ##TODO Reduce dimensionality using PCA.
reduce <- function(coef){
    
}


##TODO Select a number of model types.
  ##TODO Create models of each type, using an appropriate range of hyperparameters and from different features if desired (ie some from PCA, others from natural features)
  ##TODO Select best model.
model <- function(){
    
}

##TODO Evaluate selected model
evaluate <- function(model){
    
}
