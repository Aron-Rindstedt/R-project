run <- function() {
  cat("The aim for this function is to make it obvious that a linear model must be the best fit.\n")
  init()
  model <- lm(caps/survival.time ~l+start.level, data=rbind(train,val))
  
  new.l <- seq(1:15)
  new.level <- seq(1:50)
  pred <- matrix(nrow = 15, ncol = 50)
  for (c in 1:ncol(pred)) { #Iterating over levels
    newdata <- matrix(seq(1,15), ncol=1)
    newdata <- cbind(newdata, rep(c,15))
    newdata <- data.frame(newdata) #repetera luck=1:15 och level=r
    names(newdata) <- c("l","start.level")
    pred[,c] <- predict(model, newdata = newdata)
  }
  plot3d(test$l, test$start.level, test$caps/test$survival.time,
         xlab = "Luck", ylab = "Level", zlab = "Caps")
  surface3d(new.l, new.level, pred, alpha=.5)
  cat("If we look at the plot, we see the signs of heteroscedacitity. Since we have not learned how to deal with it in the course, we have not dealt with it.")
  cat("The estimated formula is:\nCaps/h =",
      model$coefficients[1],"+",model$coefficients[2],"* l","+",model$coefficients[3]," * start.level\n")
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
  #Installs packages, imports files and divides data
  
  packages <- installed.packages()
  if (!"rgl" %in% packages[,1])
    install.packages("rgl")
  require('rgl')
  
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
  assign("train", shelter.list[[1]], envir = .GlobalEnv)
  assign("val", shelter.list[[2]], envir = .GlobalEnv)
  assign("test", shelter.list[[3]], envir = .GlobalEnv)
}