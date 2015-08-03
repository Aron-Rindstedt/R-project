run <- function(){
    init()
    coef <- examine()
    shelter.reduced.data <- reduce(coef)
    
    shelter.list <- divide(shelter.reduced.data)
    assign("n.items.train", shelter.list[[1]], envir = .GlobalEnv)
    assign("n.items.val", shelter.list[[2]], envir = .GlobalEnv)
    assign("n.items.test", shelter.list[[3]], envir = .GlobalEnv)

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
    
    ## Import data files
    if (!exists("n.items")){
        cat("Please select the shelter_number data csv file.\n")
        n.items <- read.csv(file.choose(), header=T, sep=";")
        assign("n.items", n.items, envir = .GlobalEnv)
    }
    if (!exists("survival.time")){
      cat("Please select the shelter_survival data csv file.\n")
      survival.time <- read.csv(file.choose(), header=T, sep=";")
      survival.time <- subset(survival.time, select = -c(name))
      assign("survival.time", survival.time, envir = .GlobalEnv)
    }
}

##TODO Examine importance of features (even if you will not discard any).
examine <- function(){
  cat("Number of items vs. time, dmg and SPECIAL stats:\n")
  print(summary(lm(n~.,n.items))$coefficients)
  cat("\n\nNumber of items/h vs. dmg and SPECIAL stats:\n")
  print(summary(lm(n/time~.,n.items))$coefficients)
  cat("\n\nSurvival time vs. dmg and SPECIAL stats")
  print(summary(lm(survival.time~.,survival.time))$coefficients)
  cat("\n\nCaps/hour vs. dmg and SPECIAL stats")
  print(summary(lm(caps/survival.time~.,survival.time))$coefficients)
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
