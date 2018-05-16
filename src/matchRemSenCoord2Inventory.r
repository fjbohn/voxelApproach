#'
#' @param file character string indicated the file to read.
#' @param coord vector indicateing the coordinates in GIS: c(low.left.x, low,left.y, low.right.x low.right.y)
#' @param posOutlieres numeric height, above which return values are removed.
#' @param negOutlieres string; methode to deal with negative outliers: "correct" replaces neg. z values with 0.

matchRemSenCoord2Inventory<-function(file,coord,posOutliers=60,negOutliers="correct"){
  las <-as.data.frame(readLAS(file, short=T))
  colnames(las)<-c("X","Y","Z","intensity","returnNumber")
  ### Transforming LiDAR geographic reference of LiDAR returns to match census data ###
  # Look up the coordinates of the plot corners in a GIS
  lower.left.x <- coord[1]
  lower.left.y <- coord[2]
  lower.right.x <- coord[3]
  lower.right.y <- coord[4]
  # Set the lower left corner of the plot to coordinates [0, 0]
  trans.las <- las
  trans.las$X <- las$X - lower.left.x
  trans.las$Y <- las$Y - lower.left.y
  # Rotate the point cloud such that plot borders align with X- and Y-axes
  
  # Calculate the rotation angle to rotate a rectangular shape or point cloud
  # The arcus tangens function in R atan() provides
  # radians. To convert to degrees multiply with 90/(0.5*pi)
  
  katx <-lower.right.x - lower.left.x
  katy <- lower.left.y - lower.right.y 
  hyp <- (katx^2+katy^2)^0.5
  (rotation.angle.radians <- atan(katy/katx))
  (rotation.angle.degrees <- rotation.angle.radians*90/(0.5*pi))
  
  # Use the rotation function
  
  trans.las <- rotate.points(trans.las, -rotation.angle.degrees)
  
  # Remove outliers, caused by measuring errors (e.g. birds) or transformation errors (las tools) #
  l <- which(trans.las$Z > 60 )
  trans.las <- trans.las[-l,]
  # correct neg outliers, caused by measuring errors (e.g. birds) or transformation errors (las tools) #
  trans.las$Z<-replace(trans.las$Z,which(trans.las$Z < 0),0)
      
  
  colnames(trans.las)[1:3]<-c("x","y","z")
  
  return(trans.las)
}
