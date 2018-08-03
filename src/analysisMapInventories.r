
source("imageMapInventory.r")

sortiert <- sort.int(census$agb, index.return = T)$ix

plot(census$X[sortiert], 
     census$Y[sortiert], 
     cex = (census$dbh[sortiert] / max(census$dbh[sortiert])), 
     pch = 16, 
     col = paste0(openColours("jet", 100)[census$agb[sortiert] / max(census$agb[sortiert]) * 100], "ff"),
     xlab = "x[m]",
     ylab = "y[m]"
     )

imageMapInventory(BCI100Points, min = 0, max = max(BCI20Points$xyz[, 3]), xlab = "x[m]", ylab = "y[m]")
hist(BCI100Points$xyz[, 3], xlim = c(0, 2300), breaks = seq(0, 2300, 50), col = openColours("jet", 66), main = "", xlab = "biomass [t odm ha^-1]")

imageMapInventory(BCI50Points, min = 0, max = max(BCI20Points$xyz[, 3]), xlab = "x[m]", ylab = "y[m]")
hist(BCI50Points$xyz[, 3], xlim = c(0, 2300), breaks = seq(0, 2300, 50), col = openColours("jet", 66), main = "", xlab = "biomass [t odm ha^-1]")

imageMapInventory(BCI20Points, min = 0, max = max(BCI20Points$xyz[, 3]), xlab = "x[m]", ylab = "y[m]")
hist(BCI20Points$xyz[, 3], xlim = c(0, 2300), breaks = seq(0, 2300, 50), col = openColours("jet", 66), main = "", xlab = "biomass [t odm ha^-1]")