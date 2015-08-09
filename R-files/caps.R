run <- function() {
  init()
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
  # Import data file
  if (!exists("survival.time")){
    cat("Please select the shelter_survival data csv file.\n")
    survival.time <- read.csv(file.choose(), header=T, sep=";")
    survival.time <- subset(survival.time, select = -c(name))
    survival.time$death.level <- survival.time$death.level - survival.time$start.level
    colnames(survival.time)[11] <- "level.increase"
    assign("survival.time", survival.time, envir = .GlobalEnv)
  }
}