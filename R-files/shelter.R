run <- function(){
    init()
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

# Examine importance of features


MSE <- function(x,y){
    mean((x-y)^2)
}

model <- function(){
    t.sur <- survival.time.train
    #newdata <-
    
    #Linear model
    cat("Starting on linear model.\n")
    lin.mod <- lm(survival.time~e+l+start.level,t.sur)
    lin.pred <- predict(lin.mod, newdata = survival.time.val)
    
    #Random forest model
    cat("Starting on random forest model.\n")
    rf.mod <- randomForest(survival.time~e+l+start.level,t.sur)
    #rf.pred <- predict(rf,newdata=newdata)
    #TODO
    
    #Compare the models
    models.sur <- list(lin.mod)
  
    correct <- survival.time.val[,"survival.time"]
    errors <- lapply(models.sur,function(x)MSE(predict(x,newdata=survival.time.val),
                                               correct))

    return(models.sur[[which.min(errors)]])
}

evaluate <- function(model){
    return(list(summary(model),
               MSE(predict(model,newdata=survival.time.test),
                    survival.time.test[,"survival.time"])))
}
