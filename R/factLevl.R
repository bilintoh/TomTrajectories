FactLevel <- function(theRaster) {

  t <- ratify(theRaster)
  j <- levels(t[[1]])

  return(j)

}
