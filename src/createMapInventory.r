
#'Convert an inventory of trees attribute (e.g. AGB) into a map
#'
#' @param inventory A data.fram. Each row represents one individual stem (some tree individuals have several stems). The columns need to
#' have the following names and content: X = X-coordinate of tree position, Y = Y-coordinate of tree position. If available: CD = tree crown diameter (m), AGB = above ground biomass (t), CV= tree crown volume m³,
#' @param variable Character Sting; The target variable which will be maped
#' @param resolution Numeric; Area (m²)to which the variable will be aggregated.
#' @param sampling Character String; sampling methode: "random" or "quadraticRaster"
#' @param dist Numeric; in case of "random" the minimal distance to other patches; in case of "Raster" the distance between patches. 1 means no overlaping, so the distance between two centers of two patches is dist*radius the radius of the shape. 
#' @param sample.size numeric; only needed if sampling is random.
#' @param crown.distributed boolean whether to produce a crown distributed or stem 
#' localized AGB representation
#' @param CDParam vector; default c(18.16,0.68) calcualtes crown diameter as power law param[1]*(inventory$dbh/1000)^param[2]. IF vector longer than 2, than the values are taken as CD values.
#' @return Raster object which contains
#' @keywords raster rasterization above ground biomass AGB treelist tons crown distributed stem localized
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de
CreateMapInventory<-function(inventory,variable,resolution=10000,sampling="quadratic", dist=1, sample.size=100, shape = "square", method="stem",return.fineMAP=F, res=0.1,CDParam=c(18.16,0.68)){
  
  # debug: inventory=census; variable="AGB"
 
  
  # fine resulution Map
  factor<-1/res
  fineMap<-projectionMap(inventory,variable=variable,type=method,res=res)
  
  if(return.fineMAP){
    farbe<-openColours("jet",n=100)
    image(fineMap,col=farbe)
    IMreturn(fineMap)
    break
  }
  ##*********************************
  # patch cutting
  ##********************************
  
  #omitting edg effects trees not included in the inventory
  if(method!="stem"){
    xMax<-round(max(inventory$X[]),digits=0)-20
    yMax<-round(max(inventory$Y[]),digits=0)-20
    xMin<-20
    yMin<-20
  }else{
    xMax<-round(max(inventory$X[]),digits=0)
    yMax<-round(max(inventory$Y[]),digits=0)
    xMin<-0
    yMin<-0
  }
  # list of patches
  if(shape=="square"){
    if(sampling=="quadratic"){
      side<-sqrt(resolution)
      if(side*factor%%1!=0){
        warning("your resolution fit not into the fineMap Raster")
        side<-round(side*factor,digits=0)/factor
        warning(paste("your new adapted resolution is now:",side^2,"m²"))
      }
      if(dist<0)warning("dist must be greater than 0")
      lat<-seq(yMin,xMax-side,side*dist)
      long<-seq(yMin,yMax-side,side*dist)
      patches<-matrix(nrow=length(long)*length(lat),ncol=2)
      patches[,1]<-rep(lat,length(long))
      patches[,2]<-rep(long,each=length(lat))
    }
    if(sampling=="random"){
      cut("totdo")
    }
  }
  
  # data of patches
  maxx<-ceiling(max(inventory$X))
  maxy<-ceiling(max(inventory$Y))
  lat<-seq(0,maxx,res)
  long<-seq(maxy,0,-res)
  finexyz<-cbind(rep(lat,length(long)),rep(long,each=length(lat)),as.vector(fineMap))
  xyz<-matrix(nrow=nrow(patches),ncol=3)
  
  
  map<-list()
  finexyz<-data.table(finexyz)
  colnames(finexyz)<-c("lat","long","variable")
  if(shape=="square"){
    if(sampling == "quadratic"){
      
      for(i in 1:nrow(patches)){
        lines<-which(finexyz$lat>=patches[i,1] & finexyz$lat<patches[i,1]+side & finexyz$long>=patches[i,2] & finexyz$long<patches[i,2]+side)
        xyz[i,3]<-sum(finexyz[lines,3])*10000/resolution
        xyz[i,1:2]<-patches[i,]
      }
      map[[1]]<-xyz
      map[[2]]<-sampling
      map[[3]]<-shape
      map[[4]]<-resolution
      map[[5]]<-method
      map[[6]]<-side
      names(map)<-c("xyz","sampling","shape","resolution","method","side")
    } 
  }
  return(map)
}


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
