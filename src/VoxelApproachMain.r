# Overview of the VoxelApproach to determine Biomass on various biomes and 

DataPath <- "C:/Daten/owncloud/Projects/VoxelApproach/Daten/"
setwd("C:/Daten/owncloud/Projects/VoxelApproach/VoxelApproachRP/R/")
source("loadings.r")

############################################
# 1. match of spatial field inventories and Remote Sensing data
############################################
setwd("C:/Daten/owncloud/Projects/VoxelApproach/VoxelApproachRP/R2/")

source("collectStandarizeCensi.R")
# create object census and censi
# cenci = list of dataframes census
# colnames of census: c("x", "y", "agb", "dbh", "AGB", "h", "species", "treeID", "stemID")
# census = data of BCI

source("collectStandarizeRemSen.r")
# returns remsen
# type data.frame
# colnames x y z intensisity returnNumber.

############################################
# 2. Generate inventory- and lidar-based biomass maps
############################################

# voxel map

inventoryMaps <- list()
# inventory map
setwd("C:/Daten/owncloud/Projects/VoxelApproach/VoxelApproachRP/R2/")
source("Analyse_agb_vs_AGB.r") # compares differen calculations of tree AGB
source("createMapInventory.r") 
BCI100Points <- CreateMapInventory(census, variable = "agb", resolution = 10000, sampling = "quadratic", dist = 1, sample.size = 100, shape = "square", method = "stem", return.fineMAP = F, res=0.5)
BCI50Points <- CreateMapInventory(census, variable = "agb", resolution = 2500, sampling = "quadratic", dist = 1, sample.size = 100, shape = "square", method = "stem", return.fineMAP = F, res = 0.5)
BCI20Points <- CreateMapInventory(census, variable = "agb", resolution = 400, sampling = "quadratic", dist = 1, sample.size = 100, shape = "square", method = "stem", return.fineMAP = F, res = 0.5)

source("analysisMapInventories.r")
source("createVoxelProfile.r")

BCIv100Presence <- CreateVoxelProfile(remsen, "agb", coord = BCI100Points$xyz[, 1:2], resolution = BCI100Points$resolution, sampling = BCI100Points$sampling, dist = 1, sample.size = 100, shape = BCI100Points$shape, method="presence", return.fineMAP=F, voxelsize = 1, side = BCI100Points$resolution^0.5,MCH=T)
BCIv50Presence <- CreateVoxelProfile(remsen, "agb", coord = BCI50Points$xyz[, 1:2], resolution = BCI50Points$resolution, sampling == BCI50Points$sampling, dist = 1, sample.size = 100, shape = BCI50Points$shape, method="presence", return.fineMAP=F, voxelsize = 1, side = BCI50Points$resolution^0.5)
BCIv20Presence <- CreateVoxelProfile(remsen, "agb", coord= BCI20Points$xyz[, 1:2], resolution = BCI20Points$resolution, sampling == BCI20Points$sampling, dist = 1, sample.size = 100, shape = BCI20Points$shape, method="presence", return.fineMAP=F, voxelsize = 1, side = BCI20Points$resolution^0.5)

source(fitParameters.r)
VoxelBasedAGBMaps <- list()

BCI <- CreateAGBMapVoxels(remSen)

source(makeErrorAnalysis1hecPoint.r)
source(makeScale2Parameter.r)

############################################
# 3. Ananylsis plots
############################################
source(plotsofLidar2Scatter.r)
source(plotErrorAnalysis.r)
source(plotScale2Parameter.r)