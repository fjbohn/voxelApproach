
#'Convert an  of trees attribute (e.g. AGB) into a map
#'
#'@param remsen data.frame: containing las data: colnames have to be: "x","
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
  
  #
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
  
#' ATTANTION only 1 m³ voxel allowed
#' @param coord vector()
#' @param side numeric. side variable of map
#' @param shape character string, type of shape
#' @param method character string, type of method; "presence count all occupied voxels
#' @param count.above numeric. threshold of minimal height of returnpoint to be counted.
findVoxelDistribution<-function(remsen,coord,side, shape, method="presence",count.above=1,sens=0.05,k=0.25){
  if(method=="presence"){
    if(shape=="square"){
      lines<-which(remsen[,1]>=coord[1] & remsen[,1]<coord[1]+side & remsen[,2]>=coord[2] & remsen[,2]<coord[2]+side & remsen[,3]>count.above)
      xyz<-ceiling(remsen[lines,1:3])-0.5
      
    }
    xyz<-table(xyz)
    xyz<-replace(xyz,which(xyz>1),1)
    zett<-as.numeric(names(xyz[1,1,]))
  }
  if(method=="beamTransformed"){
    if(shape=="square"){
      xyz<-makeBeamTransformedLidar2Voxel(remsen,coord,sens=sens,k=k,side=side,cut.out=count.above)
      zett<-1:dim(xyz)[3]+0.5
    }
  }
  if(method=="vfp10"){
    if(shape=="square"){
      highresolution<-matrix(nrow=60,ncol=side^2/(10*10),data=0)
      for(i in 1:(side^2/(10*10) )){
        x<-floor(i/10)
        y<-(i-1)%%10
        lines<-which(remsen[,1]>=coord[1]+x & remsen[,1]<coord[1]+side+x & remsen[,2]>=coord[2]+y & remsen[,2]<coord[2]+side+y & remsen[,3]>count.above)
        xyz<-ceiling(remsen[lines,1:3])-0.5
        xyz<-table(xyz)
        xyz<-replace(xyz,which(xyz>1),1)
        profile2<-as.numeric(names(xyz[1,1,]))
        
        zett<-vertical.foliage.profile(profile2, h.bin = 1, GR.threshold = 5, k = 0.5)
        highresolution[1:length(zett),i]<-zett
      }
      voxelDist<-matrix(nrow = nrow(highresolution),ncol=2)
      for( i in 1:nrow(highresolution)){
        voxelDist[i,1]<-i-0.5
        voxelDist[i,2]<-sum(highresolution[i,])
      }
    }
  }

  if(method!="vfp10"){
    voxelDist<-matrix(nrow=max(zett),ncol=2)
    
    for( i in 1:max(zett)){
      level<-which(zett==i-0.5)
      voxelDist[i,1]<-zett[i]
      voxelDist[i,2]<-sum(xyz[,,level])#*0.4*1000
    }
  }
  
  
  return(voxelDist)
}

calcMCH<-function(remsen,coord){
  lines<-which(remsen[,1]>=coord[1] & remsen[,1]<coord[1]+side & remsen[,2]>=coord[2] & remsen[,2]<coord[2]+side & remsen[,3]>count.above)
  mch<-mean(remsen[lines,3])
  return(mch)
  
}
