run <- function() {
  init()
  
  plot3d(survival.time$l, survival.time$start.level, survival.time$caps/survival.time$survival.time,
         xlab = "Luck", ylab = "Level", zlab = "Caps")
  cat("If we look at the plot, we see the signs of possible heteroscedacitity.",
      "\nThus, we compare a linear model to generalized models with Gamma family error distribution.",
      "\nWe are comparing 1st order and 2nd order polynomials w.r.t. luck.\n")
  . <- readline("Press [ENTER] to create the model and plot test points and regression surface.")
  
  lin.model <- lm(caps/survival.time ~l+start.level, data=train)
  lin.g.model <- glm(caps/survival.time ~l+start.level, family=Gamma, data=train)
  quad.g.model <- glm(caps/survival.time ~ poly(l,2) + start.level, family=Gamma, data=train)
  
  lin.error <-  mse(predict(lin.model, newdata = val), val$caps/val$survival.time)
  lin.g.error <-  mse(predict(lin.g.model, newdata = val), val$caps/val$survival.time)
  quad.g.error <-mse(predict(quad.g.model, newdata = val), val$caps/val$survival.time)
  
  cat("MSEs:",
      "\nLinear model:",lin.error,
      "\nGeneral linear model:", lin.g.error,
      "\nQuadratic general linear model:", quad.g.error,
      "\nThe ordinary linear model performs best.")
  
  model <- lin.model
  
  new.l <- seq(1:15)
  new.level <- seq(1:50)
  pred <- matrix(nrow = 15, ncol = 50)
  for (c in 1:ncol(pred)) { #Iterating over levels
    newdata <- matrix(seq(1,15), ncol=1)
    newdata <- cbind(newdata, rep(c,15))
    newdata <- data.frame(newdata) #repetera luck=1:15 och level=r
    names(newdata) <- c("l","start.level")
    pred[,c] <- predict(model, newdata = newdata, type="response")
  }
  plot3d(test$l, test$start.level, test$caps/test$survival.time,
         xlab = "Luck", ylab = "Level", zlab = "Caps")
  surface3d(new.l, new.level, pred, alpha=.5)
}

mse <- function(x,y){
  mean(x-y)^2
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