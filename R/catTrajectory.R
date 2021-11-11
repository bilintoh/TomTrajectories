catTrajectory <- function(dfRaster,noData = 0, category) {
  # Filter pixel locations with values == 0, which is NA
  pixelColumns <- length(dfRaster[1,])
  dfRaster[,3:pixelColumns][dfRaster[,3:pixelColumns] == as.numeric(noData)] <- NA
  dfNa <- dfRaster %>% filter_all(any_vars(is.na(.)))
  naXY <- dfNa[1:2]
  naXYChange <- naXY %>% mutate(change = 0)

  # Filter Data which is not NA
  dfNonZero <- dfRaster[complete.cases(dfRaster), ]
  nonNaXY <- dfNonZero[, 1:2]
  dfNonZero2 <- dfNonZero[, 3:pixelColumns]
  lenDfNonZero2 <- length(dfNonZero2)

  # extend length of pixel column by 1 and 2
  pixelColumnsBy1 <- pixelColumns + 1
  pixelColumnsBy2 <- pixelColumns + 2

  # convert selected columns to a Boolean dataframe where 0 = absence and 1 = presence
  dfNonZero2[dfNonZero2 != as.numeric(category)] <- as.numeric(0)
  dfBoolean2 <- dfNonZero2
  dfBoolean2[dfBoolean2 == as.numeric(category)] <- as.numeric(1)

  # Combined coordinates and columns without NA
  dfXYZBoolean <- cbind(nonNaXY,dfBoolean2)


  #Create the five trajectories that make up the map
  # 0 = Mask, 1 = Absence, 2 = Presence, 3 = Gain, and 4 = Loss

  #1 = Absence
  absence <- dfXYZBoolean %>% mutate(change = ifelse(
    .[3] == 0 &.[pixelColumns] == 0 ,1,0))

  absence <-absence %>% subset(.$change == 1) %>% subset(., select=c("x", "y", "change"))

  #2 = Presence
  presence <- dfXYZBoolean %>% mutate(change = ifelse(
    .[3] == 1 &.[pixelColumns] == 1 ,2,0))

  presence <-presence %>% subset(.$change == 2) %>% subset(., select=c("x", "y", "change"))

  #Compute the difference between the last and first time points
  lastFirstTimepoints <- dfXYZBoolean[pixelColumns] - dfXYZBoolean[3]

  xylastFirstTimepoints<- cbind(nonNaXY,lastFirstTimepoints)

  xylastFirstTimepoints2 <- xylastFirstTimepoints %>% filter(lastFirstTimepoints!=0)


  dfReclass <- xylastFirstTimepoints2 %>% mutate(change = ifelse(.[3] == 1,3,4))


  dfReclass2 <- data.frame(dfReclass$x,dfReclass$y,dfReclass$change)

  # name the columns of the new dataframe x,y, and change
  names(dfReclass2) <- c("x","y","change")

  # Put the 4 component into 1 data frame
  noNaComponents <- rbind(absence,presence,dfReclass2)

  # Join NA, comp_1,comp_2, and comp_3_4_v2

  combinedTrajectory <- rbind(naXYChange,absence,presence,dfReclass2)

  #factCombinedTrajectory <- as.factor(rasterFromXYZ(as.data.frame(combinedTrajectory)))







  return(list("combinedTrajectory" = combinedTrajectory,
              "dfXYZBoolean" = dfXYZBoolean))
}



