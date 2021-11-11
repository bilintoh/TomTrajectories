stackedBarData <- function(cattrajectory,timePoints,pixelSize = 1000) {

  dfXYZBoolean <- as.data.frame(cattrajectory$dfXYZBoolean)

  numColumns <- length(dfXYZBoolean)

  dfXYZBoolean2 <- dfXYZBoolean[, 3:numColumns]

  numbTimePoints <- length(dfXYZBoolean2)


  # Create time intervals
  pairTimepoints <- data.frame(cbind(timePoints[-length(timePoints)], timePoints[-1]))
  fromToTimepoints <- paste(pairTimepoints$X1, "to", pairTimepoints$X2)

  # Create size of time intervals
  sizeTimeInterval<- pairTimepoints[-1] - pairTimepoints[-ncol(pairTimepoints)]

  # Create temporal extent
  temporalExtent <- timePoints[[numbTimePoints]]-timePoints[[1]]

  # Create last time interval
  lastInterval <- paste(head(timePoints,n=1), "TO", tail(timePoints,n=1))

  # Create number of time intervals
  numbTimeIntervals <- numbTimePoints - 1


  #Remove rows with all numbers == 0 0r 1
  dfXYZBoolean3<- dfXYZBoolean2[rowSums(dfXYZBoolean2[]) > 0,] # absence

  dfXYZBoolean4<- dfXYZBoolean3[rowSums(dfXYZBoolean3[]) < numbTimePoints,] # persistence

  trajectory <- dfXYZBoolean4[-1] - dfXYZBoolean4[ - ncol(dfXYZBoolean4)] # 1= gains, -1=losses

  trajectoryGains <- as.data.frame(colSums(trajectory == 1)) # sum gain for each tim e inetrval

  trajectoryLoss <- as.data.frame(colSums(trajectory == - 1) * -1)# sum loss for each tim e inetrval

  trajectoryGainLoss <- cbind(sizeTimeInterval,trajectoryGains,trajectoryLoss)

  names(trajectoryGainLoss) <- c("interval","gain","loss")

  trajectoryGainLossNet <- trajectoryGainLoss %>% mutate(net = .$gain + .$loss)


  sqkilometer <- pixelSize^2/1000^2

  # Divide gain, loss, and net by their intervals and multiply by area
  trajectoryGainLossNet2 <- round((
    trajectoryGainLossNet[-1]/
      (trajectoryGainLossNet$interval)) * sqkilometer,4)


  trajectoryGainLossNet2 <- cbind(
    sizeTimeInterval,trajectoryGainLossNet2,fromToTimepoints) # Bring back the time interval


  maxGain <- max(trajectoryGainLossNet2$gain)

  maxLoss <- max(abs(trajectoryGainLossNet2$loss))

  # Create label for stacked bar's vertical axis
  labVerticalAxis <- ifelse(
    maxGain >= 1000 |
      maxLoss >= 1000,
    "Change (thousand sqaure Kilometers per year)",
    "Change (sqaure Kilometers per year)")

  if(labVerticalAxis == "Change (sqaure Kilometers per year)"){
    dfStackedBar <- trajectoryGainLossNet2

  }else{
    dfStackedBar <- trajectoryGainLossNet2[,2:4]/ (1000)

    dfStackedBar <- cbind(sizeTimeInterval,dfStackedBar,fromToTimepoints)

  }


  #Create data for stacked bars without net
  dfStackedBar2 <- dfStackedBar


  dfStackedBar2$net <- NULL



  trajdfStackedBar <- melt(dfStackedBar2,id = c("X2","fromToTimepoints"))

  #Create data for stacked bars with net

  dfStackedBar3 <- dfStackedBar

  dfStackedBar4 <- subset(dfStackedBar3,select =  -c(gain,loss)) # select all except gain and loss

  net_loss <-  dfStackedBar4 %>% subset(.$net < 0)

  colnames(net_loss)[2] <- "net loss"

  net_gain <- dfStackedBar4 %>% subset(.$net > 0)

  colnames(net_gain)[2] <- "net gain"

  dfCombinedNet <- merge(net_gain,net_loss, all = TRUE)

  dfCombinedNet[is.na(dfCombinedNet)] <- 0

  dfCombinedNet2 <- melt(dfCombinedNet,id = c("X2","fromToTimepoints"))


  # Compute gross gain and gross loss
  grossGain <- sum(dfStackedBar$gain)/(numbTimePoints - 1) # compute gross gain


  grossLoss <- sum(dfStackedBar$loss)/(numbTimePoints - 1) # compute gross loss

  grossPosition <- levels(trajdfStackedBar$fromToTimepoints)

  grossPosition <- grossPosition[1]


  return(list("NonetData" = trajdfStackedBar,
              "NetData" = dfCombinedNet2,
              "GrossGain" = grossGain,
              "GrossLoss" = grossLoss,
              "yaxisLable" = labVerticalAxis,
              "lastTimeInterval" = lastInterval,
              "numbTimeIntervals" = numbTimeIntervals))


}
