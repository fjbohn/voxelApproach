#' calcualtes a fine resolution map
#' @param nventory inventory x,y,...
#' @param res numeric. In meters (e.g 0.1)
#' @param variable name of the target variable
#' calcualtes a fine resolution map
#' @param inventory inventory x,y,...
#' @param param ector; default c(18.16,0.68) calcualtes crown diameter as power law param[1]*(inventory$dbh/1000)^param[2]. IF vector longer than 2, than the values are taken as CD values.
#' 
projectionMap<-function(inventory,variable,res=0.5,type="sphere",param=c(18.16,0.68)){
  CD<-param[1]*(inventory$dbh/1000)^param[2]
  factor<-1/res
  maxx<-ceiling(max(inventory$X))*factor
  maxy<-ceiling(max(inventory$Y))*factor
  
  collumn<-which(colnames(inventory)==variable)
  treelist<-as.data.table(cbind(inventory$X*factor,inventory$Y*factor,CD*factor,inventory[,collumn]))
  colnames(treelist)<-c("X","Y","CD","AGB")
  
  if(type=="stem"){
    fineMap<-biomass.raster.from.treelist.dt(
      treelist,
      minx = 0,
      maxx = maxx,
      miny = 0,
      maxy = maxy,
      dist="stem" 
    )
  }
  if(type=="circle"){
    fineMap<-biomass.raster.from.treelist.dt(
      treelist,
      minx = 0,
      maxx = maxx,
      miny = 0,
      maxy = maxy,
      dist="uniform"
    )
  }
  if(type=="sphere"){
    fineMap<-biomass.raster.from.treelist.dt(
      treelist,
      minx = 0,
      maxx = maxx,
      miny = 0,
      maxy = maxy,
      dist="sphere",
      sphere.fraction=1
    )
    #image(fineMap,col=openColours("jet",100))
  }
  if(type=="gausian"){
    fineMap<-biomass.raster.from.treelist.dt(
      treelist,
      minx = 0,
      maxx = maxx,
      miny = 0,
      maxy = maxy,
      dist="Gaussian"
    )
  }
  
  return(fineMap)
}
