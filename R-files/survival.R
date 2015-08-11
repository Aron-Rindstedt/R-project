run <- function(){
    init()
    chosen.model <- model()
    . <- readline("Press [ENTER] to evaluate chosen model.")
    evaluate(chosen.model)
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
    # Install and load packages
    packages <- installed.packages()
    if (!"randomForest" %in% packages[,1])
      install.packages("randomForest")
    require('randomForest')
    
    # Import data file
    if (!exists("survival.time")){
      cat("Please select the shelter_survival data csv file.\n")
      survival.time <- read.csv(file.choose(), header=T, sep=";")
      survival.time <- subset(survival.time, select = -c(name))
      survival.time$death.level <- survival.time$death.level - survival.time$start.level
      colnames(survival.time)[11] <- "level.increase"
      
      assign("survival.time", survival.time, envir = .GlobalEnv)
    }
    
    shelter.list <- divide(survival.time)
    assign("survival.time.train", shelter.list[[1]], envir = .GlobalEnv)
    assign("survival.time.val", shelter.list[[2]], envir = .GlobalEnv)
    assign("survival.time.test", shelter.list[[3]], envir = .GlobalEnv)
}

msle <- function(x,y){
    #Returns the mean of square difference between logs
    #we want to penalize errors for small values
    mean((log(x)-log(y))^2)
}

polyfit <- function(var, deg) {
  training.data <- survival.time.train
  newdata <- survival.time.val
  correct <- survival.time.val[,"survival.time"]
  
  deg.e <- 1
  deg.l <- 1
  deg.level <- 1
  if (var == "e")
    deg.e <- deg
  if (var == "l")
    deg.l <- deg
  if (var == "level")
    deg.level <- deg
  
  model <- lm(survival.time~poly(e,deg.e)+poly(l,deg.l)+poly(start.level,deg.level)+start.damage,
              data = training.data)
  pred <- predict(model, newdata = newdata)
  error <- msle(pred, correct)
  return(list(model,error))
}

polyfit.e <- function(deg) {
  polyfit("e", deg)
}

polyfit.l <- function(deg) {
  polyfit("l", deg)
}

polyfit.level <- function(deg) {
  polyfit("level", deg)
}

model <- function(){
    training.data <- survival.time.train
    correct <- survival.time.val[,"survival.time"]
    newdata <- survival.time.val
    
    #Linear model
    cat("Starting with linear model.\n")
    lin.mod <- lm(survival.time~e+l+start.level+start.damage,
                  data = training.data)
    lin.pred <- predict(lin.mod,
                        newdata = newdata)
    lin.error <- msle(lin.pred, correct)
    errors <- lin.error
    names(errors)[length(errors)] <- "Linear"
    
    
    #Random forest model
    cat("Starting with random forest model.\n")
    rf.mod <- randomForest(survival.time~e+l+start.level+start.damage,
                           training.data)
    rf.pred <- predict(rf.mod,
                       newdata=newdata)
    rf.error <- msle (rf.pred, correct)
    errors <- c(errors, rf.error)
    names(errors)[length(errors)] <- "Random forest"
    
    #Polynomials over e
    cat("Starting with polynomials over E.\n")
    e2.fit <- polyfit.e(2)
    e2.mod <- e2.fit[[1]]
    e2.error <- e2.fit[[2]]
    e3.fit <- polyfit.e(3)
    e3.mod <- e3.fit[[1]]
    e3.error <- e3.fit[[2]]
    errors <- c(errors, e2.error)
    names(errors)[length(errors)] <- "E-squared"
    errors <- c(errors, e3.error)
    names(errors)[length(errors)] <- "E-cubed"
    
    #Polynomials over l
    cat("Starting with polynomials over L.\n")
    l2.fit <- polyfit.l(2)
    l2.mod <- l2.fit[[1]]
    l2.error <- l2.fit[[2]]
    l3.fit <- polyfit.l(3)
    l3.mod <- l3.fit[[1]]
    l3.error <- l3.fit[[2]]
    errors <- c(errors, l2.error)
    names(errors)[length(errors)] <- "L-squared"
    errors <- c(errors, l3.error)
    names(errors)[length(errors)] <- "L-cubed"
    
    #Polynomials over start.level
    cat("Starting with polynomials over start.level.\n")
    level2.fit <- polyfit.level(2)
    level2.mod <- level2.fit[[1]]
    level2.error <- level2.fit[[2]]
    level3.fit <- polyfit.level(3)
    level3.mod <- level3.fit[[1]]
    level3.error <- level3.fit[[2]]
    errors <- c(errors, level2.error)
    names(errors)[length(errors)] <- "Level-squared"
    errors <- c(errors, level3.error)
    names(errors)[length(errors)] <- "Level-cubed"
    
    #Compare the models
    models <- list(lin.mod, rf.mod, 
                   e2.mod, e3.mod, 
                   l2.mod, l3.mod,
                   level2.mod, level3.mod)
    cat("Model creation done!\n===================\n\n")
    new.order <- order(errors)
    errors <- errors[new.order]
    models <- models[new.order]
    cat("Validation mean squared logarithmic errors:\n")
    cat("&\\textbf{Model}&\\textbf{MSLE}\\\\\\hline\n")
    for (i in 1:length(errors))
      cat(i,"&",names(errors)[i],"&",errors[i],"\\\\\n")
    cat("\\hline\n")
    cat("End of validation\n===================\n\n")
    
    min.error.ind <- which.min(errors)
    
    cat("Chosen model:", names(errors)[min.error.ind],"\n")
    
    return(models[[min.error.ind]])
}

evaluate <- function(model){
  pred <- predict(model,newdata=survival.time.test)
  correct <- survival.time.test[,"survival.time"]
  cat("Estimated MSLE for the selected model on test data:",msle(pred,correct),"\n")
  . <- readline("Press [ENTER] to see the summary of the selected model.")
  print(summary(model))
}
