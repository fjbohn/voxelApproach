
setwd("C:/Daten/owncloud/Projects/VoxelApproach/VoxelApproachRP/R/")
source("matchRemSenCoord2Inventory.r")

# 1 BCI

# 1.1 load las data
setwd(DataPath)

remsen <- matchRemSenCoord2Inventory("area_with_more_density_norm.las", c(625773.8, 1011775.9, 626773.3, 1011743.5))

#display.point.cloud(remSen[which(remSen$x < 30 & remSen$y < 30), ], col.var = "z", col.lim = c(120, 220))

# 2 Traunstein
# 1.1 load census data
# 1.2 standardize Coordinates
# 1.3 additional infos

# 1.2) Read only the most important columns // TERRAIN INCLUDED
#pc <- readLAS("lasclip_50ha_plot.las", short=T)
#pc <- data.frame(pc)
#head(pc)
#ncol(pc)
#nrow(pc)