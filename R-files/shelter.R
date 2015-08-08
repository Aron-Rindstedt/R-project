run <- function(){
    init()
    coef <- examine()
    ## reduced.num <- reduce.num()
    reduced.sur <- reduce.sur()
    
    ## shelter.list <- divide(reduced.num)
    ## assign("n.items.train", shelter.list[[1]], envir = .GlobalEnv)
    ## assign("n.items.val", shelter.list[[2]], envir = .GlobalEnv)
    ## assign("n.items.test", shelter.list[[3]], envir = .GlobalEnv)

    shelter.list <- divide(reduced.sur)
    assign("survival.time.train", shelter.list[[1]], envir = .GlobalEnv)
    assign("survival.time.val", shelter.list[[2]], envir = .GlobalEnv)
    assign("survival.time.test", shelter.list[[3]], envir = .GlobalEnv)
    
    models <- model()
    return(evaluate(models))
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
    
    # Import data files
    if (!exists("n.items")){
        cat("Please select the shelter_number data csv file.\n")
        n.items <- read.csv(file.choose(), header=T, sep=";")
        assign("n.items", n.items, envir = .GlobalEnv)
    }
    if (!exists("survival.time")){
      cat("Please select the shelter_survival data csv file.\n")
      survival.time <- read.csv(file.choose(), header=T, sep=";")
      survival.time <- subset(survival.time, select = -c(name))
      survival.time$death.level <- survival.time$death.level - survival.time$start.level
      colnames(survival.time)[11] <- "level.increase"
      assign("survival.time", survival.time, envir = .GlobalEnv)
    }
}

# Examine importance of features (even if you will not discard any).
examine <- function(){
  cat("\n\nNumber of items/h vs. dmg and SPECIAL stats:\n")
  print(summary(lm(n/time~.,n.items))$coefficients)
  cat("\n\nSurvival time vs. dmg and SPECIAL stats:\n")
  print(summary(lm(survival.time~.,survival.time))$coefficients)
  cat("\n\nCaps vs. time, dmg and SPECIAL stats:\n")
  print(summary(lm(caps~.,survival.time))$coefficients)
  cat("\n\nCaps/hour vs. dmg and SPECIAL stats:\n")
  print(summary(lm(caps/survival.time~.,survival.time))$coefficients)
}

##Irrelevant since the item-finding thing is already determined.
reduce.num <- function(){
    n.items
}

##DONE If too many features:
##DONE Select those of low importance from step 2 to remove.
##DONE Reduce dimensionality using PCA.
reduce.sur <- function(){
    survival.time[,c("e","l","start.level","survival.time")]
}

MSE <- function(x,y){
    mean((x-y)^2)
}

model <- function(){
    ##TODO Select a number of model types.
    ##TODO Create models of each type, using an appropriate range of hyperparameters and from different features if desired (ie some from PCA, others from natural features)
    ## t.num <- n.items.train
    ## models.num <- list(lm(n~.,t.num))

    t.sur <- survival.time.train
    models.sur <- list(lm(survival.time~.,t.sur))

    ##DONE? Select best model.
    ## error.num <- lapply(models.num,function(x)MSE(predict(x,newdata=n.items.val),
    ##                                               n.items.val[,"n"]))
    error.sur <- lapply(models.sur,function(x)MSE(predict(x,newdata=survival.time.val),
                                                  survival.time.val[,"survival.time"]))

    return(list(models.sur[[which.min(error.sur)]]))
}

evaluate <- function(models){
    ## model.num <- models[[1]]
    ## model.sur <- models[[2]]
    model.sur <- models[[1]]

    return(list(summary(model.sur),
               MSE(predict(model.sur,newdata=survival.time.test),
                    survival.time.test[,"survival.time"])))
}
