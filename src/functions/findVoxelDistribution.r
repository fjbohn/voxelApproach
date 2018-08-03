#' ATTENTION only 1 m³ voxel allowed
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

