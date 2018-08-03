
#'Convert an  of trees attribute (e.g. AGB) into a map
#'
#' @param remsen data.frame: containing las data: colnames have to be: "x","
#' @param method character string: "beamTransformed","presence","points","vfl"
#' @param variable Character Sting; The target variable which will be maped
#' @param resolution Numeric; Area (m²)to which the variable will be aggregated.
#' @param sampling Character String; sampling methode: "random" or "quadraticRaster"
#' @param dist Numeric; in case of "random" the minimal distance to other patches; in case of "Raster" the distance between patches. 1 means no overlaping, so the distance between two centers of two patches is dist*radius the radius of the shape. 
#' @param crown.distributed boolean whether to produce a crown distributed or stem 
#' localized AGB representation
#' @return Raster object which contains
#' @keywords raster rasterization above ground biomass AGB treelist tons crown distributed stem localized
#' @export
#' @examples in progress
#' @author Friedrich J. Bohn
CreateVoxelProfile<-function(remsen,coord=NA,variable,resolution=10000,sampling="quadratic", dist=1, sample.size=100, shape = "square", method="presence",return.fineMAP=F,voxelsize=1,side=10000^0.5,sens=0.2,k=0.3,MCH=T){
  voxelDist<-list()
  mch<-vector()
  for( i in 1:nrow(coord)){
    # Count amount of hits in a voxel of a given dimension #
    if(method=="presence") voxelDist[[i]]<-findVoxelDistribution(remsen,coord[i,1:2],side, shape, method="presence")
    if(method=="beamTransformed") voxelDist[[i]]<-findVoxelDistribution(remsen,coord[i,1:2],side, shape, method="beamTransformed",sens=sens,k=k)
    if(method=="vfp10") voxelDist[[i]]<-findVoxelDistribution(remsen,coord[i,1:2],side, shape, method="vfp10")

    if(MCH==T)mch[i]<-calcMCH(remsen,coord[i,1:2])

    names(voxelDist)[i]<-paste(coord[i,1],coord[i,2])
  }
  
  # output object
  data<-list()
  data[[1]]<-voxelDist
  data[[2]]<-sampling
  data[[3]]<-shape
  data[[4]]<-resolution
  data[[5]]<-method
  data[[6]]<-side
  names(data)[1:6]<-c("voxelProfile","sampling","shape","resolution","method","side")
  if(MCH){
    data[[7]]<-mch
    names(data)[7]<-"mch"
  }  
  
  return(data)
}
