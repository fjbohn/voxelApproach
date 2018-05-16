
# data structure
censi<-list()



# 1 BCI
BCI<-list()
# 1.1 load census data
setwd(DataPath)
census <-read.csv ("InventoryTable_census_7.txt", header = T, sep =";")
census <- data.frame(census$gx,census$gy,census$agb,census$dbh,census$AGB,census$height,census$sp,census$treeID,census$stemID)# and only alive
colnames(census)[1:9]<-c("X","Y","agb","dbh","AGB","h","species","treeID","stemID")
# 2 Traunstein
# 1.1 load census data
# 1.2 standardize Coordinates
# 1.3 additional infos