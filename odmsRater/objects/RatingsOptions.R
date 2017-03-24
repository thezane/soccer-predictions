newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    k=1,
    c=0.2,
    strBetas=c(1, 1),
    corrBeta=-1,
    meanGoals=1,
    hA=0.2,
    xpDefault=1,
    fTree=fTree,
    fNames=fNames,
    numFs=numFs,
    tolRel=0.01,
    tolScale=0.01
  )
  
  class(rOptions) <- "RatingsOptions"
  rOptions
}

updateOptions <- function(rOptions, k, c, strBetas, corrBeta,
      meanGoals, hA, strFsNorm) {
  rOptions$k <- k
  rOptions$c <- c
  rOptions$strBetas <- strBetas
  rOptions$corrBeta <- corrBeta
  rOptions$meanGoals <- meanGoals
  rOptions$hA <- hA
  strFs <- exp(strFsNorm)
  i <- 1
  
  while (i <= rOptions$numFs) {
    strF <- strFs[i]
    rOptions$fTree[[rOptions$fNames[i]]] <- c(strF, 1 / strF)
    i <- i + 1
  }

  rOptions
}

printModel <- function(rOptions) {
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("atk = %f", rOptions$strBetas[1])))
  print(noquote(sprintf("def = %f", rOptions$strBetas[2])))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("ha = %f", rOptions$hA)))
  i <- 1
  
  while (i <= rOptions$numFs) {
    print(noquote(sprintf("%s = %f", rOptions$fNames[i],
        log(rOptions$fTree[[rOptions$fNames[i]]][1]))))
    i <- i + 1
  }
}
