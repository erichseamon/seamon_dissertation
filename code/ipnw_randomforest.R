#ipnw_randomforest.R
#
#Erich Seamon - University of Idaho
#PHD dissertation code for random forest modeling of ipnw insurance loss
#in relationship to climate data
#Summer 2019


library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
library(mvnormtest)
library(plotly)
# but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]
library(maptools)
library(MASS)
library(dplyr)
#------

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) 
{ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits = digits)[1] 
  txt <- paste0(prefix, txt) 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt) 
  text(0.5, 0.5, txt, cex = cex.cor * r) 
} 


#load wheat pricing

wheatprice <- read.csv("/dmine/data/USDAprices/wheatprices_1998_2017.csv", header=TRUE, strip.white =TRUE)
wheatprice_year <- aggregate(wheatprice$Price, list(wheatprice$Year), FUN="mean")
colnames(wheatprice_year) <- c("year", "price")

wheatprice <- read.csv("/dmine/data/USDAprices/wheat_prices_revised_1989_2015.csv", header=TRUE, strip.white =TRUE)
wheatprice <- wheatprice[-1]

colnames(wheatprice) <- c("year", "price")


#use a cube transformation on loss for WHEAT claims

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#--accessing output of design matrix/time lag data based on monthly selection from dashboard runs

var1 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_jun2_cube_root_loss_climatecorrelation.csv")
colnames(var1)[9] <- paste(colnames(var1)[2], "_zscore", sep="")
var2 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_jul3_cube_root_loss_climatecorrelation.csv")
colnames(var2)[9] <- paste(colnames(var2)[2], "_zscore", sep="")
var3 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_cube_root_loss_climatecorrelation.csv")
colnames(var3)[9] <- paste(colnames(var3)[2], "_zscore", sep="")
var4 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_jun2_cube_root_acres_climatecorrelation.csv")
colnames(var4)[9] <- paste(colnames(var4)[2], "_zscore", sep="")

var5 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_jun2_loss_per_acre_climatecorrelation.csv")
colnames(var5)[9] <- paste(colnames(var5)[2], "_zscore", sep="")
var6 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jun1_cube_root_acres_climatecorrelation.csv")
colnames(var6)[9] <- paste(colnames(var6)[2], "_zscore", sep="")


var7 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_sep5_loss_climatedata.csv")
colnames(var7)[9] <- paste(colnames(var7)[2], "_zscore", sep="")


var8 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_sep5_loss_climatedata.csv")
colnames(var8)[9] <- paste(colnames(var8)[2], "_zscore", sep="")
var9 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_loss_climatedata.csv")
colnames(var9)[9] <- paste(colnames(var9)[2], "_zscore", sep="")

var9x <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/fm100_jul3_cube_root_loss_climatedata.csv")
colnames(var9x)[9] <- paste(colnames(var9x)[2], "_zscore", sep="")

var10x <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/fm1000_aug2_cube_root_loss_climatedata.csv")
colnames(var10x)[9] <- paste(colnames(var10x)[2], "_zscore", sep="")

var11x <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pdsi_sep4_cube_root_loss_climatedata.csv")
colnames(var11x)[9] <- paste(colnames(var11x)[2], "_zscore", sep="")

var12x <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pdsi_sep4_cube_root_loss_climatecorrelation.csv")
colnames(var12x)[9] <- paste(colnames(var12x)[2], "_zscore", sep="")

var7a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_sep5_loss_climatecorrelation.csv")
colnames(var7a)[9] <- paste(colnames(var7a)[2], "_zscore", sep="")


var8a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_sep5_loss_climatecorrelation.csv")
colnames(var8a)[9] <- paste(colnames(var8a)[2], "_zscore", sep="")
var9a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_loss_climatecorrelation.csv")
colnames(var9a)[9] <- paste(colnames(var9a)[2], "_zscore", sep="")


#revised combined variables

var13 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/WHEAT_Drought_tmmx_cube_root_loss.csv")
var14 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/WHEAT_Drought_pr_cube_root_loss.csv")
var15 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/WHEAT_Drought_pet_cube_root_loss.csv")

data6 <- cbind(var13[,2], var14[,2], var15[,2], var15[,3], data.frame(var15[,4]), data.frame(var15[,5]), var15[,6])
colnames(data6) <- c("tmmx", "pr", "pet", "year", "county", "state", "cube_loss")
data6 <- left_join(data6, wheatprice_year, by = c("year"))

#scaling climate data

data6$pr_scaled <- scale(data6$pr, scale = TRUE, center = TRUE)
data6$tmmx_scaled <- scale(data6$tmmx, scale = TRUE, center = TRUE)
data6$pet_scaled <- scale(data6$pet, scale = TRUE, center = TRUE)
data6$price_scaled <- scale(data6$price, scale = TRUE, center = TRUE)
data6 <- na.omit(data6)

#generates a pairwise plot 
pairs(cube_loss ~ pr_scaled + tmmx_scaled + pet_scaled, data = data6, col = data6$state,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")

par(mar=c(12.7, 6.1, 4, 2.1), family = 'serif', mgp=c(4, 1, 0), las=0)

#biplots of climate data vs insurance loss by county
ggplot(data4aa, aes(pet_scaled, cube_loss, color = state)) + 
  geom_point(aes(size = cube_loss^3)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Potential Evapotranspiration (mm)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))

ggplot(data4aa, aes(pr_scaled, cube_loss, color = state)) + 
  geom_point(aes(size = cube_loss^3)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Precipitation (mm)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))


ggplot(data4aa, aes(tmmx_scaled, cube_loss, color = state)) + 
  geom_point(aes(size = cube_loss^3)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Max Temperature (C)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))

ggplot(data4aa, aes(prpet, cube_loss, color = state)) + 
  geom_point(aes(size = cube_loss^3)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Aridity Index (PR / PET)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))


#----secondary analysis using non optimized


data1 <- cbind(var1, var2[9], var3[9])
data2 <- cbind(var1[1:6], var2[2], var3[2])

data3 <- cbind(var4[1:6], var5[2], var6[2])

data3 <- plyr::join(data3, wheatprice_year, by = "year")

data4 <- cbind(var7[1:6], var8[2], var9[2], var9x[2], var10x[2], var11x[2], var12x[3] )
#data4$prpet <- data4$pr / data4$pet
data4a <- left_join(data4, var7a, by = c("year" = "year", "county" = "county"))
data4a <- left_join(data4a, wheatprice, by = c("year"))
data4aa <- na.omit(data4a)

colnames(data4aa) <- c("X", "pr", "year", "pr_zscore", "damagecause", "county", "pet", "tmmx", "fm100", "fm1000", "pdsi", "cube_loss", "X.y", "pr2", "loss", "state", "commodity", "matrixnumber", "clim_zscore", "loss_zscore", "price")

#creating aridity index variable
data4aa$prpet <- data4aa$pr / data4aa$pet

data4aa <- subset(data4aa, , c(year, damagecause, county, commodity, state, matrixnumber, loss, cube_loss, pr, pdsi, pet, tmmx, fm100, fm1000, price, prpet))
data4aa$log_loss <- log10(data4aa$loss)

#scaling
data4aa$pr_scaled <- scale(data4aa$pr, scale = TRUE, center = TRUE)
data4aa$tmmx_scaled <- scale(data4aa$tmmx, scale = TRUE, center = TRUE)
data4aa$pet_scaled <- scale(data4aa$pet, scale = TRUE, center = TRUE)
data4aa$price_scaled <- scale(data4aa$price, scale = TRUE, center = TRUE)
data4aa$pdsi_scaled <- scale(data4aa$pdsi, scale = TRUE, center = TRUE)

#--plotting results of individual variables for loss
pairs(log_loss ~ pr_scaled + tmmx_scaled + pet_scaled + pdsi_scaled, data = data4aa, col = data4aa$state,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")

par(mar=c(12.7, 6.1, 4, 2.1), family = 'serif', mgp=c(4, 1, 0), las=0)

#biplots
ggplot(data4aa, aes(pet_scaled, log_loss, color = state)) + 
  geom_point(aes(size = price)) + scale_radius() +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Potential Evapotranspiration (mm)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))

ggplot(data4aa, aes(pr_scaled, log_loss, color = state)) + 
  geom_point(aes(size = price)) + scale_radius() +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Precipitation (mm)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))


ggplot(data4aa, aes(tmmx_scaled, log_loss, color = state)) + 
  geom_point(aes(size = price)) + scale_radius() +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Max Temperature (C)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))

ggplot(data4aa, aes(pdsi_scaled, log_loss, color = state)) + 
  geom_point(aes(size = price)) + scale_radius() +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Aridity Index (PR / PET)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))

ggplot(data4aa, aes(prpet, log_loss, color = state)) + 
  geom_point(aes(size = price)) + scale_radius() +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Aridity Index (PR / PET)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))


#---adding pct_acreage

#percentage acreage by county and year, per state

library(plotrix)
library(ggplot2)
library(gridExtra)


setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

ID1 <- aggregate(.~countyfips + year, ziggy.df, sum)
ID1 <- aggregate(.~countyfips, ID1, mean)

setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

WA1 <- aggregate(.~countyfips + year, ziggy.df, sum)
WA1 <- aggregate(.~countyfips, WA1, mean)

setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

OR1 <- aggregate(.~countyfips + year, ziggy.df, sum)
OR1 <- aggregate(.~countyfips, OR1, mean)

countiesfips <- read.csv("/dmine/data/counties/counties_fips.csv", header = TRUE)
colnames(countiesfips) <- c("countyfips", "county", "state")

OR2 <- merge(countiesfips, OR1, by=("countyfips"))
ID2 <- merge(countiesfips, ID1, by=("countyfips"))
WA2 <- merge(countiesfips, WA1, by=("countyfips"))

#--OR

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = paste("Oregon", "_summary", sep=""))
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)


#--acres for all damage causes aggregated
OR_loss_commodity <- subset(xrange, commodity == var_commodity)
OR_loss_all <- aggregate(OR_loss_commodity$acres, by=list(OR_loss_commodity$year, OR_loss_commodity$county, OR_loss_commodity$state), FUN = "sum")
colnames(OR_loss_all) <- c("year", "county", "state", "all_damagecause_acres")

OR_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
OR_loss2 <- aggregate(OR_loss1$loss, by=list(OR_loss1$year, OR_loss1$county, OR_loss1$state), FUN = "sum")
OR_loss2_acres <- aggregate(OR_loss1$acres, by=list(OR_loss1$year, OR_loss1$county, OR_loss1$state), FUN = "sum")
colnames(OR_loss2) <- c("year", "county", "state", "loss")
colnames(OR_loss2_acres) <- c("year", "county", "state", "acres")

OR_loss2$acres <- OR_loss2_acres$acres
OR_loss2$lossperacre <- OR_loss2$loss / OR_loss2$acres


OR_loss_climate <- merge(OR_loss2, OR2[-4], by=c("county", "state"))
OR_loss_climate_2 <- merge(OR_loss_all, OR_loss_climate, by=c("year", "county", "state"))


#-WA

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = paste("Washington", "_summary", sep=""))
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)


#--acres for all damage causes aggregated
WA_loss_commodity <- subset(xrange, commodity == var_commodity)
WA_loss_all <- aggregate(WA_loss_commodity$acres, by=list(WA_loss_commodity$year, WA_loss_commodity$county, WA_loss_commodity$state), FUN = "sum")
colnames(WA_loss_all) <- c("year", "county", "state", "all_damagecause_acres")

WA_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
WA_loss2 <- aggregate(WA_loss1$loss, by=list(WA_loss1$year, WA_loss1$county, WA_loss1$state), FUN = "sum")
WA_loss2_acres <- aggregate(WA_loss1$acres, by=list(WA_loss1$year, WA_loss1$county, WA_loss1$state), FUN = "sum")
colnames(WA_loss2) <- c("year", "county", "state", "loss")
colnames(WA_loss2_acres) <- c("year", "county", "state", "acres")

WA_loss2$acres <- WA_loss2_acres$acres
WA_loss2$lossperacre <- WA_loss2$loss / WA_loss2$acres


WA_loss_climate <- merge(WA_loss2, WA2[-4], by=c("county", "state"))
WA_loss_climate_2 <- merge(WA_loss_all, WA_loss_climate, by=c("year", "county", "state"))

#-ID



setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = paste("Idaho", "_summary", sep=""))
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)


#--acres for all damage causes aggregated
ID_loss_commodity <- subset(xrange, commodity == var_commodity)
ID_loss_all <- aggregate(ID_loss_commodity$acres, by=list(ID_loss_commodity$year, ID_loss_commodity$county, ID_loss_commodity$state), FUN = "sum")
colnames(ID_loss_all) <- c("year", "county", "state", "all_damagecause_acres")

ID_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
ID_loss2 <- aggregate(ID_loss1$loss, by=list(ID_loss1$year, ID_loss1$county, ID_loss1$state), FUN = "sum")
ID_loss2_acres <- aggregate(ID_loss1$acres, by=list(ID_loss1$year, ID_loss1$county, ID_loss1$state), FUN = "sum")
colnames(ID_loss2) <- c("year", "county", "state", "loss")
colnames(ID_loss2_acres) <- c("year", "county", "state", "acres")

ID_loss2$acres <- ID_loss2_acres$acres
ID_loss2$lossperacre <- ID_loss2$loss / ID_loss2$acres


ID_loss_climate <- merge(ID_loss2, ID2[-4], by=c("county", "state"))
ID_loss_climate_2 <- merge(ID_loss_all, ID_loss_climate, by=c("year", "county", "state"))


merged_iPNW <- rbind(ID_loss_climate_2, WA_loss_climate_2, OR_loss_climate_2)
merged_iPNW$pct_acreage <- merged_iPNW$acres / merged_iPNW$all_damagecause_acres


#

#--plotting results of individual variables for pct acreage

pairs(cube_loss ~ pr_scaled + tmmx_scaled + pet_scaled + pdsi_scaled, data = data4aaaa, col = data4aaaa$state,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot", cex.labels=1.9, cex.cor = 4.4, pch = 19, cex = .5, font.labels = 2, cex.axis = 1.5)


par(mar=c(12.7, 6.1, 4, 2.1), family = 'serif', mgp=c(4, 1, 0), las=0)

data4aaaa <- merge(data4aa, merged_iPNW, by = c("state", "county", "year"))

ggplot(data4aaaa, aes(pet_scaled, cube_loss, color = state)) + 
  geom_point(aes(size = price)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Potential Evapotranspiration (mm)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))

ggplot(data4aaaa, aes(pr_scaled, cube_loss, color = state)) + 
  geom_point(aes(size = price)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Precipitation (mm)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))


ggplot(data4aaaa, aes(tmmx_scaled, cube_loss, color = state)) + 
  geom_point(aes(size = price)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled Max Temperature (C)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))

ggplot(data4aaaa, aes(prpet, cube_loss, color = state)) + 
  geom_point(aes(size = price)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Aridity Index (PR / PET)", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))


ggplot(data4aa, aes(pdsi, cube_loss, color = state)) + 
  geom_point(aes(size = price)) +
  stat_smooth(aes(group = 1), method = "loess") + labs(x = "Scaled PDSI", y = "Cube root loss ($)") + theme(axis.title.y = element_text(family = "Serif", size=16)) + theme(axis.title.x = element_text(family = "Serif", size=16), axis.text.x = element_text(size=rel(1.5), hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.5), hjust = 1, family = "Serif"))





#-----







#--setting up fit


fit <- lm(cube_loss ~ pet_scaled + pr_scaled * price_scaled, data = data4aa)


colnames(data3) <- c("X", "pr", "acres", "year", "state", "county", "pet", "tmmx")

cor2 <- cor(data1) 
pairs(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore, data = data1,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")

#--acres
pairs(acres ~ pr + tmmx + pet, data = data3,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")

#--loss
pairs(loss ~ pr + tmmx + pet, data = data3a,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")
#--end

#--aov test
fit <- aov(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore, data = data1)

fit <- aov(loss ~ pr + tmmx + pet, data = data2)

layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests


par(mfrow=c(1,1))
#--Two-way Interaction Plot 

data3a <- subset(data2, year >= 2001)
data4b <- subset(data4a, year >= 2001)
#attach(mtcars)
county <- factor(data4b$county)
year <- factor(data4b$year)
interaction.plot(year, county, data4b$pr, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(1:25),	
                 fun = mean,
                 xlab=" ", 
                 ylab="mean PR (mm)", 
                 main="Interaction Plot - PRECIP vs. year for palouse region counties, by county", las = 2)

data3a <- subset(data2, year >= 2001)

#attach(mtcars)
county <- factor(data3a$county)
year <- factor(data3a$year)
colnames(data3a) <- c("X", "pr", "loss", "year", "state", "county", "pet", "tmmx")

interaction.plot(year, county, data3a$tmmx, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(1:25),	
                 xlab=" ", 
                 ylab="TMMX (K)", 
                 main="Interaction Plot - TMMX vs. year for PNW states", las = 2)


#attach(mtcars)
county <- factor(data3$state)
year <- factor(data3$year)
interaction.plot(year, county, data3$acres, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(1:25),	
                 xlab=" ", 
                 ylab="acres", 
                 main="Interaction Plot - Wheat/Drought loss (acres) vs. year for PNW states", las = 2)


fit <- lm(cube_loss ~ pr + tmmx + pet + fm100 + fm1000, data = data4aa)

library(rpart)
library(rpart.plot)

# Make big tree
form <- as.formula(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore)

form2 <- as.formula(cube_loss ~ pr + tmmx + pet + pdsi + price)
form2a <- as.formula(loss ~ pr + tmmx + pet  + price)
form2b <- as.formula(cube_loss ~ pr.x + tmmx.x + pet.x + pdsi.x + price)


#form3 <- as.formula(pct_acreage ~ pr + tmmx + pet + pdsi + sph + srad + vs + th + erc + fm100 + fm1000 + price)

form3 <- as.formula(pct_acreage ~ pr + tmmx + pet + pdsi + price)
form3a <- as.formula(pct_acreage ~ pr + tmmx + pet + pdsi + sph + srad + vs + th + erc + fm100 + fm1000 + price)

tree.1 <- rpart(form2b,data=data4aaaa,control=rpart.control(minsplit=30,cp=0))
# 
rpart.plot(tree.1)					# Will make a mess of the plot
text(tree.1, cex = .5)
# 
prp(tree.1)					# Will plot the tree
prp(tree.1,varlen=5)				# Shorten variable names

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1) # display the new tree
#
#-------------------------------------------------------------------

data44a <- data.frame(data4aaaa)

data5a_statecountyear <- cbind.data.frame(data44a$state, data44a$county, data44a$year)
colnames(data5a_statecountyear) <- c("state", "county", "year")

data44a$tmmx.x <- data44a$tmmx.x - 273

data5a <- cbind.data.frame(data44a$year, data44a$cube_loss, data44a$loss.x, data44a$pr_scaled, data44a$tmmx_scaled, data44a$fm100.x, data44a$fm1000.x, data44a$pdsi_scaled, data44a$erc, data44a$srad, data44a$vs, data44a$sph, data44a$th,
                           data44a$pet_scaled, data44a$pct_acreage, data44a$price_scaled, data44a$state)
colnames(data5a) <- c("year", "cube_loss", "loss", "pr", "tmmx", "fm100", "fm1000", "pdsi", "erc", "srad", "vs", "sph", "th", "pet", "pct_acreage", "price", "state" )

data5a <- data.frame(data5a)
data5a$loss <- log10(data5a$loss)

data5ab <- cbind.data.frame(data5a$loss, data5a$pr, data5a$tmmx, data5a$pdsi, data5a$pet, data5a$price)
colnames(data5ab) <- c("loss", "pr", "tmmx", "pdsi", "pet", "price")
data5ab <- data.frame(data5ab)

data5ab$loss <- log10(data5ab$loss)

# load libraries
library(caret)
library(rpart)

# define training control
train_control<- trainControl(method="repeatedcv", number=10, savePredictions = TRUE)

data2b <- data.frame(data2b)
data2b <- na.omit(data2b)

data4b <- data.frame(data4b)
data4b <- na.omit(data4b)

data5b <- data.frame(data5a)
data5b <- na.omit(data5b)

data5ab <- data.frame(data5ab)
data5ab <- na.omit(data5ab)






pairs(loss ~ pr_scaled + tmmx_scaled + pet_scaled + pdsi_scaled, data = data5b, col = data5b$state,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")

# train the model
set.seed(9992)
#trainIndex  <- sample(1:nrow(data5b), 0.8 * nrow(data5b))

trainIndex <- createDataPartition(data5b$pct_acreage, p = .75, list = FALSE)

train <- data5b[trainIndex,]
test <- data5b[-trainIndex,]

trainIndex_loss <- caret::createDataPartition(data5ab$loss, p = .75, list = FALSE)

train_loss <- data5ab[trainIndex_loss,]
test_loss <- data5ab[-trainIndex_loss,]

#traindata=data.frame(x=train_loss,y=(train_loss))
#testdata = data.frame(x=test_loss,y=(test_loss))


rf_grid <- expand.grid(mtry = 2:5) # you can put different values for mtry


model<- caret::train(form3, data=data5b, trControl=train_control, importance=T, method="rf", tuneGrid = rf_grid, ntrees = 500)
model_singular <- caret::train(form3, data=train, trControl=train_control, method="rpart")

model_loss <- caret::train(form2a, data=train_loss, trControl=train_control, method="rf", tuneGrid = rf_grid, ntrees = 1000, importance = T)
model_loss_all <- caret::train(form2a, data=data5b, trControl=train_control, method="rf", ntrees = 500, tuneGrid = rf_grid, importance = T)
model_loss_all_acreage <- caret::train(form3, data=data5b, trControl=train_control, method="rf", ntrees = 500, tuneGrid = rf_grid, importance = T)

cforest_model_loss <- cforest(form2a,
                                     data = train_loss, 
                                     controls=cforest_unbiased(ntree=2000, mtry=5))


lattice::densityplot(model_loss,
            adjust = 1.25)

tree_num <- which(model_loss$finalModel$err.rate[, 1] == min(model_loss$finalModel$err.rate[, 1]))

lda_data <- learing_curve_dat(dat = model$trainingData, 
                              outcome = ".outcome",
                              ## `train` arguments:
                              metric = "RMSE",
                              trControl = train_control,
                              method = "rf")

pts <- pretty(lda_data$RMSE)
#pts <- c(0,0.1, 0.2, 0.3, 0.4)

lda_data$Data[lda_data$Data == "Resampling"] <- c("Validation")
ggplot(lda_data, aes(x = Training_Size, y = RMSE, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()  + ggtitle("Random Forest Learning Curves: Train vs. Validation") + theme(axis.title.y = element_text(family = "Serif", size=18), axis.title.x = element_text(family = "Serif", size = 18), axis.text.x = element_text(size=rel(1.9), angle = 90, hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.9), hjust = 1, family = "Serif")) + theme(plot.title = element_text(family = "Serif", vjust = 2))  + theme(legend.text=element_text(family = "Serif", size=14)) + theme(legend.title=element_text(family = "Serif", size=16, face = "bold")) + theme(plot.title = element_text(size=24, face = "bold")) + scale_y_continuous(labels = pts, breaks = pts ) + xlab("Training Size") + ylab("RMSE") + theme(legend.position="bottom") + scale_fill_discrete(name = "Legend")

sqrt(model_loss$finalModel$mse[which.min(model_loss$finalModel$mse)])

which.min(model_loss$finalModel$mse)

importance <- varImp(model_loss, scale=FALSE)

importance2 <- importance$importance
importance2$variable <- rownames(importance2)
importance2 <- importance2[order(-importance2$Overall),] 

par(mar=c(6, 7, 2, 3), family = 'serif', mgp=c(5, 1, 0), las=0)

barplot(sort(importance2$Overall), horiz = TRUE, col = "lightblue", ylab = "predictor variables", xlab = "% importance", cex.lab = 1.7, las = 2, cex.axis = 1.8, cex.names = 1.8, names.arg = rev(importance2$variable))

#NRMSE

nrmse_func <-  function(obs, pred, type = "sd") {
  
  squared_sums <- sum((obs - pred)^2)
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  if (type == "sd") nrmse <- rmse/sd(obs)
  if (type == "mean") nrmse <- rmse/mean(obs)
  if (type == "maxmin") nrmse <- rmse/ (max(obs) - min(obs))
  if (type == "iq") nrmse <- rmse/ (quantile(obs, 0.75) - quantile(obs, 0.25))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
  nrmse <- round(nrmse, 3)
  return(nrmse)
  
}

library(caretEnsemble)

algorithmList <- c('gbm', 'rpart', 'ctree', 'rf', 'HYFIS', 'FS.HGD', 'ANFIS' )

set.seed(100)
models <- caretList(form2, data=data5b, trControl=train_control, methodList=algorithmList) 
results <- resamples(models)
summary(results)

set.seed(101)
stackControl <- train_control

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="glm", trControl=stackControl)
print(stack.glm)

#fit <- randomForest(form3, data = data5b)

# make predictions
predictions<- predict(model,data5b)

predictions_loss <- predict(model_loss,data5b)

cforest_Prediction <- predict(cforest_model_loss, newdata=test_loss, OOB=TRUE, type = "response")



predictions_loss_all <- predict(model_loss_all,data5b)


#table comparing predicitons to truth

table(predict=predictions_loss, truth=test_loss)


# append predictions
mydat<- cbind(data5b,predictions)
mydat_loss <- cbind(data5b,predictions_loss)
cforest_mydat_loss <- cbind(test_loss,cforest_Prediction)

mydat_loss_all <- cbind(data5b,predictions_loss_all)

# summarize results
#confusionMatrix<- confusionMatrix(mydat$predictions,mydat$loss)







#--plotting example counties individually

finaldat <- cbind.data.frame(data5a_statecountyear, mydat)
finaldat_loss <- cbind.data.frame(data5a_statecountyear, mydat_loss)
finaldat_loss_all <- cbind.data.frame(data5a_statecountyear, mydat_loss_all)

NRMSE <- nrmse_func(finaldat$loss, finaldat$predictions)

NRMSE <- nrmse_func(finaldat_loss$loss, finaldat_loss$predictions_loss)


fit1 <- lm(form3, data=data5b)

#pct_acreage

countyplot <- subset(finaldat, county == "Whitman")

par(mar=c(5, 5.1, 2, 3), family = 'serif', mgp=c(3.5, 1, 0), las=0)


plot(c(countyplot$pct_acreage) ~ c(countyplot$year), cex.lab = 1.5, cex.axis = 1.3, col = "red", 
     las = 2, xaxt = "n", xlab = "Year", ylab = "Percent Acreage Drought Claims")

lines(countyplot$pct_acreage ~ countyplot$year, col = "red")
points(countyplot$predictions ~ countyplot$year, col = "blue")
lines(countyplot$predictions ~ countyplot$year, col = "blue")

axis(1, countyplot$year, 2001:2015, col.axis = "black", las = 2, cex.axis = 1.3)

legend("topright", legend=c("historical", "predicted"),
       col=c("red", "blue"), lty=1, cex=1.3)

#loss
finaldat_loss$loss <- 10^finaldat_loss$loss
finaldat_loss$predictions_loss <- 10^finaldat_loss$predictions_loss
fd_loss <- aggregate(finaldat_loss$loss, by=list(finaldat_loss$year), FUN = "sum")
fd_pred <- aggregate(finaldat_loss$predictions_loss, by=list(finaldat_loss$year), FUN = "sum")

#plot all counties combined, by year
par(mar=c(6, 5.1, 2, 3), family = 'serif', mgp=c(3.5, .6, 0), las=0)


plot(fd_loss$x ~ fd_loss$Group.1, col = "red", cex.lab = 1.5, cex.axis = 1.3, las = 2, yaxt = "n", xaxt = "n", xlab = "Year", ylab = "Annual Wheat/Drought Loss", main = "24 county iPNW study area")
lines(fd_loss$x ~ fd_loss$Group.1, col = "red")
points(fd_pred$x ~ fd_pred$Group.1, col = "blue")
lines(fd_pred$x ~ fd_pred$Group.1, col = "blue")

axis(1, fd_loss$Group.1, 2001:2015, col.axis = "black", las = 2, cex.axis = 1.3)

pts <- pretty(fd_loss$x / 1000000)
pts2 <- pretty(fd_loss$x)
axis(2, las = 1, cex.axis = 1.3, at = pts2, labels = paste(pts, "M", sep = ""))



legend("topleft", legend=c("historical", "predicted"),
       col=c("red", "blue"), lty=1, cex=1.3)

#----

#plot all counties combined, by year - cforest test
par(mar=c(6, 5.1, 2, 3), family = 'serif', mgp=c(3.5, .6, 0), las=0)


plot(fd_loss$x ~ fd_loss$Group.1, col = "red", cex.lab = 1.5, cex.axis = 1.3, las = 2, yaxt = "n", xaxt = "n", xlab = "Year", ylab = "Annual Wheat/Drought Loss", main = "24 county iPNW study area")
lines(fd_loss$x ~ fd_loss$Group.1, col = "red")
points(fd_pred$x ~ fd_pred$Group.1, col = "blue")
lines(fd_pred$x ~ fd_pred$Group.1, col = "blue")

axis(1, fd_loss$Group.1, 2001:2015, col.axis = "black", las = 2, cex.axis = 1.3)

pts <- pretty(fd_loss$x / 1000000)
pts2 <- pretty(fd_loss$x)
axis(2, las = 1, cex.axis = 1.3, at = pts2, labels = paste(pts, "M", sep = ""))



legend("topleft", legend=c("historical", "predicted"),
       col=c("red", "blue"), lty=1, cex=1.3)

#----


countyplot <- subset(finaldat_loss, county == "Adams")
yearlist <- data.frame(c(2001:2015))
colnames(yearlist) <- c("year")

countyplot <- merge(yearlist,
                    countyplot, by = c("year"), all.x = TRUE)

countyplot[is.na(countyplot)] <- 0

countyplot_all <- subset(finaldat_loss_all, county == "Adams")

par(mar=c(5, 6, 2, 3), family = 'serif', mgp=c(3.5, .6, 0), las=0)


plot(c(10^countyplot$loss) ~ c(countyplot$year), cex.lab = 1.5, cex.axis = 1.3, col = "red", las = 2, yaxt = "n", xaxt = "n", xlab = "Year", ylab = "Annual Wheat/Drought Loss", main = "Adams County, WA")

lines(10^countyplot$loss ~ countyplot$year, col = "red")
points(10^countyplot$predictions_loss ~ countyplot$year, col = "blue")
lines(10^countyplot$predictions_loss ~ countyplot$year, col = "blue")
#points(10^countyplot_all$predictions_loss_all ~ countyplot_all$year, col = "darkgreen")
#lines(10^countyplot_all$predictions_loss_all ~ countyplot_all$year, col = "darkgreen")


axis(1, countyplot$year, 2001:2015, col.axis = "black", las = 2, cex.axis = 1.3)

pts <- pretty(10^countyplot$loss / 1000000)
pts2 <- pretty(10^countyplot$loss)
axis(2, las = 1, cex.axis = 1.5, at = pts2, labels = paste(pts, "M", sep = ""))



legend("topleft", legend=c("historical", "predicted"),
       col=c("red", "blue"), lty=1, cex=1.3)




par(mar=c(5, 5.5, 2, 3), family = 'serif', mgp=c(3.5, 1, 0), las=0)
plot(varImp(model_loss))


library(ROCR)


trainIndex  <- sample(1:nrow(data5b), 0.8 * nrow(data5b))
train <- data5b[trainIndex,]
test <- data5b[-trainIndex,]

data5b$loss <- 10^data5b$loss

ctrl = rpart.control(maxdepth=8)
tree.2 <- rpart(form2,data5b, method = "anova", control=ctrl)	

tree.3 <- rpart(form3,data5b, method = "anova", control=ctrl)

rpart.plot(tree.2, digits = -3)

rpart.plot(tree.3, digits = -3)

prp(tree.3, extra=1, faclen=0,  nn=T, box.palette="Blues")


rpart(formula = cube_loss ~ pr_scaled + tmmx_scaled + pet_scaled + price_scaled, data = train, method = "anova", 
      control = rpart.control(minsplit = 0, minbucket = 1, cp = -1))

opt_index <- which.min(tree.3$cptable[, "xerror"])
cp_opt <- tree.3$cptable[2, "CP"]
tree1_opt <- prune(tree = tree.3, 
                   cp = 2)

rpart.plot(x = tree1_opt, yesno = 2, type = 0, extra = 0)




trainIndex  <- sample(1:nrow(data4aa), 0.8 * nrow(data4aa))
train <- data4aa[trainIndex,]
test <- data4aa[-trainIndex,]

ctrl = rpart.control(maxdepth=4)

tree.1 <- rpart(formula = cube_loss ~ pet + tmmx + pr, data = train, method = "anova", 
                control = rpart.control(minsplit = 0, minbucket = 1, cp = -1))





#Gradient boosed trees




library(gbm)
gbm_tree1 <- gbm::gbm(pct_acreage ~ pet + tmmx + pr + price ,data = train, distribution = "gaussian",n.trees = 10000,
                      shrinkage = 0.01, interaction.depth = 4)

plot(gbm_tree1,i="price") 

predictions <- predict(gbm_tree1, data5b, n.trees = 100)


n.trees = seq(from=1 ,to=1000, by=1) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm_tree1,test,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(test,apply( (predmatrix-pct_acreage)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)


#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.err),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)









tree.2 <- rpart(form2,data2, method = "anova", control=ctrl)	

tree.3 <- rpart(form2,data4a, method = "anova", control=ctrl)


rpart.plot(tree.3, digits = -3)





# A more reasonable tree
prp(tree.1)                                     # A fast plot													
fancyRpartPlot(tree.1)	

rpart.plot(tree.1, digits = -3)

#--loss
pairs(loss ~ pr + tmmx + pet, data = data2,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot - cube root loss transformation")


library(caret)

tc <- trainControl("cv",10)
rpart.grid <- expand.grid(.cp=0.5)

data2b <- cbind(data2$loss, data2$pet, data2$pr, data2$tmmx)
colnames(data2b) <- c("loss", "pet", "pr", "tmmx")
data2b <- data.frame(data2b)

train.rpart <- train(loss ~ ., data=na.omit(data2b), method="rpart",trControl=tc,tuneGrid=rpart.grid)





# A fancy plot from rattle
#
#-------------------------------------------------------------------
#sends to json for use with d3
jsontree <- json_prsr(tree.1)
setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/d3_tree_1/", sep=""))
jsontree2 <- gsub("'", '"', jsontree)
write(jsontree2, file="palousetree.JSON")



#------additional tree
# Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(loss ~ pr + tmmx + pet + price, 
             method="anova", data=data3)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree palouse spring wheat ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
     title = "Regression Tree palouse spring wheat ")


#---random forest

library(randomForest)

rfit <- randomForest(cube_loss ~ pr + tmmx + pet,  data=na.omit(data4aa))
print(rfit) # view results 
importance(rfit) # importance of each predictor

getTree(rfit, 1, labelVar=TRUE)

#--count data

count(train, 'damagecause')

regre <- mgcv::gam(pet_zscore ~ pr_zscore + tmmx_zscore, data=data1)
VIF1 <- (1/(1-.89))

#manova

manova(loss ~ pr + tmmx + pet, data = data2)
res.man <- manova(cbind(pet, pr, tmmx, year) ~ county*loss, data = data2)
