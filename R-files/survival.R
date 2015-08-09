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
    
    shelter.list <- divide(survival.time)
    assign("survival.time.train", shelter.list[[1]], envir = .GlobalEnv)
    assign("survival.time.val", shelter.list[[2]], envir = .GlobalEnv)
    assign("survival.time.test", shelter.list[[3]], envir = .GlobalEnv)
}

mse <- function(x,y){
    mean((x-y)^2)
}

model <- function(){
    training.data <- survival.time.train
    correct <- survival.time.val[,"survival.time"]
    newdata <- survival.time.val
    errors <- c()
    
    #Linear model
    cat("Starting on linear model.\n")
    lin.mod <- lm(survival.time~e+l+start.level+start.damage,
                  data = training.data)
    lin.pred <- predict(lin.mod,
                        newdata = newdata)
    lin.error <- mse(lin.pred, correct)
    models <- list(lin.mod)
    errors <- lin.error
    names(errors)[length(errors)] <- "Linear"
    
    #Random forest model
    cat("Starting on random forest model.\n")
    rf.mod <- randomForest(survival.time~e+l+start.level+start.damage,
                           training.data)
    rf.pred <- predict(rf.mod,
                       newdata=newdata)
    rf.error <- mse (rf.pred, correct)
    models <- c(models, rf.mod)
    errors <- c(errors, rf.error)
    names(errors)[length(errors)] <- "Random forest"
    
    #
    
    #Compare the models
    cat("Model creation done!\n===================\n\n")
    new.order <- order(errors)
    errors <- errors[new.order]
    models <- models[new.order]
    cat("Validation MS errors:\n")
    for (i in 1:length(errors))
      cat(i,":",names(errors)[i],":",errors[i],"\n")
    cat("End of validation\n===================\n\n")
    
    min.error.ind <- which.min(errors)
    
    cat("Chosen model:", names(errors)[min.error.ind])
    
    return(models[[min.error.ind]])
}

evaluate <- function(model){
  pred <- predict(model,newdata=survival.time.test)
  correct <- survival.time.test[,"survival.time"]
    return(list(summary(model),
               mse(pred,
                    correct)))
}
