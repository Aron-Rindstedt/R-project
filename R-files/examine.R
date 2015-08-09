run <- function() {
  init()
  examine()
}

init <- function() {
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

examine <- function(){
  cat("\n\nNumber of items/h vs. dmg and SPECIAL stats:\n")
  print(summary(lm(n/time~.,n.items))$coefficients)
  cat("\n\nCaps/hour vs. dmg and SPECIAL stats:\n")
  print(summary(lm(caps/survival.time~.,survival.time))$coefficients)
  cat("\n\nSurvival time vs. dmg and SPECIAL stats:\n")
  print(summary(lm(survival.time~.,survival.time))$coefficients)
}