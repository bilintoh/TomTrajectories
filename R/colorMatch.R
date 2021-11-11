

colorMatch <- function(rasList) {
  dfCatTrajectory <- rasList[1]
  rasterTrajectories <-
    rasterFromXYZ(as.data.frame(dfCatTrajectory))

  ID <- c(0,1,2,3,4)
  myColv1 <- c("White","#c4c3c0","#666666","#000080","#8b0000")
  trajetoryCalss_v1 <- c("Mask","Absence","Presence","Gain","Loss")

  #create names for trajectories
  #trajectoryNames <- c("Mask","Absence","Presence","Gain","Loss")

  idColorTrajectory <- data.frame(ID,myColv1,trajetoryCalss_v1)

  factRasterTrajectories <- ratify(rasterTrajectories)

  factRasterTrajectories2 <- factRasterTrajectories

  factRasterTrajectories2 <- levels(factRasterTrajectories2)[[1]]

  factRasterTrajectories3 <- left_join(factRasterTrajectories2,idColorTrajectory , by = c("ID" = "ID"))

  return(factRasterTrajectories3)

}
