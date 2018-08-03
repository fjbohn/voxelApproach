
calcMCH <- function(remsen, coord) {
  
  lines <- which(remsen[, 1] >= coord[1] 
                 & remsen[, 1] < coord[1]+side 
                 & remsen[, 2] >= coord[2] 
                 & remsen[, 2] < coord[2]+side 
                 & remsen[, 3] > count.above)
  
  mch <- mean(remsen[lines, 3])
  
  return(mch)
}