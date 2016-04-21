catimpact <- function(xcol, targetcol) {
  x_impact <- rep(0, length(xcol))
  for (i in unique(xcol)) {
    x_impact[xcol == i] <- mean(targetcol[xcol == i]) 
  }
  return(x_impact)
}
