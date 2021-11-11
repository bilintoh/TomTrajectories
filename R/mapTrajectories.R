#' Title
#'
#' @param rasList
#' @param mapTitle
#' @param titleFont
#' @param legendFont
#'
#' @return
#' @export
#'
#' @examples
mapTrajectories <- function(rasList,mapTitle = "MAP OF CHANGE",titleFont = 1.3, legendFont = 1.3
) {


  dfRasterTrajectories <- as.data.frame(rasList$combinedTrajectory)

  rasterTrajectories2 <- rasterFromXYZ(dfRasterTrajectories)

  # Colors and names for a generalized legend
  myColv2 <- c('White','#c4c3c0','#666666', "#000080", "#8b0000")
  trajetoryCalss_v1 <- c("Mask","Absence","Presence","Gain","Loss")

  mapCol <- colorMatch(rasList)


  # Base R plot function to plot the map. This is relatively faster than ggplot
  trajectorypPlot <- plot_noaxes(rasterTrajectories2,col = as.character(mapCol$myColv1),
                                 legend = FALSE)
  title(mapTitle, line = 0.2,cex.main = titleFont)

  # Add legend
  legend("bottomright", title = "Legend",
         legend = trajetoryCalss_v1,
         fill = myColv2,
         border = TRUE,
         bty = "n",
         cex = legendFont,
         text.font = 2) # turn off legend borde

}
