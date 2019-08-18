ipnw_climate_county_comparison <- function(var_year, var_commodity, var_damage, unit) {

library(plotrix)



setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

ID1 <- aggregate(ziggy.df$bi, by=list(ziggy.df$countyfips, ziggy.df$year), FUN = "mean")

ID1 <- aggregate(.~countyfips + year, ziggy.df, mean)

setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

WA1 <- aggregate(.~countyfips + year, ziggy.df, mean)

setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

OR1 <- aggregate(.~countyfips + year, ziggy.df, mean)

countiesfips <- read.csv("/dmine/data/counties/counties_fips.csv", header = TRUE)
colnames(countiesfips) <- c("countyfips", "county", "state")

OR2 <- merge(countiesfips, OR1, by=("countyfips"))
ID2 <- merge(countiesfips, ID1, by=("countyfips"))
WA2 <- merge(countiesfips, WA1, by=("countyfips"))


#------

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


ID_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
ID_loss2 <- aggregate(ID_loss1$loss, by=list(ID_loss1$county, ID_loss1$year, ID_loss1$state), FUN = "sum")
ID_loss2_acres <- aggregate(ID_loss1$acres, by=list(ID_loss1$county, ID_loss1$year, ID_loss1$state), FUN = "sum")
colnames(ID_loss2) <- c("county", "year", "state", "loss")
colnames(ID_loss2_acres) <- c("county", "year", "state", "acres")

ID_loss2$acres <- ID_loss2_acres$acres
ID_loss2$lossperacre <- ID_loss2$loss / ID_loss2$acres


ID_loss_climate <- merge(ID_loss2, ID2, by=c("county", "state", "year"))


#---WA

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


WA_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
WA_loss2 <- aggregate(WA_loss1$loss, by=list(WA_loss1$county, WA_loss1$year, WA_loss1$state), FUN = "sum")
WA_loss2_acres <- aggregate(WA_loss1$acres, by=list(WA_loss1$county, WA_loss1$year, WA_loss1$state), FUN = "sum")
colnames(WA_loss2) <- c("county", "year", "state", "loss")
colnames(WA_loss2_acres) <- c("county", "year", "state", "acres")

WA_loss2$acres <- WA_loss2_acres$acres
WA_loss2$lossperacre <- WA_loss2$loss / WA_loss2$acres


WA_loss_climate <- merge(WA_loss2, WA2, by=c("county", "state", "year"))


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


OR_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
OR_loss2 <- aggregate(OR_loss1$loss, by=list(OR_loss1$county, OR_loss1$year, OR_loss1$state), FUN = "sum")
OR_loss2_acres <- aggregate(OR_loss1$acres, by=list(OR_loss1$county, OR_loss1$year, OR_loss1$state), FUN = "sum")
colnames(OR_loss2) <- c("county", "year", "state", "loss")
colnames(OR_loss2_acres) <- c("county", "year", "state", "acres")

OR_loss2$acres <- OR_loss2_acres$acres
OR_loss2$lossperacre <- OR_loss2$loss / OR_loss2$acres


OR_loss_climate <- merge(OR_loss2, OR2, by=c("county", "state", "year"))





#-merge all three states

iPNW_loss_climate <- rbind(OR_loss_climate, ID_loss_climate, WA_loss_climate)

#--subset to only iPNW 24 counties

Franklin <- subset(iPNW_loss_climate, county == "Franklin" & state == "WA")

iPNW_loss_climate <- subset(iPNW_loss_climate, county == "Benewah" 
                             | county == "Latah" | county == "Nez Perce" | county == "Lewis" 
                             | county == "Idaho" | county == "Wasco" | county == "Sherman" 
                             | county == "Gilliam" | county == "Morrow" | county == "Umatilla" 
                             | county == "Union" | county == "Wallowa" | county == "Douglas" 
                             | county == "Walla Walla" | county == "Benton" | county == "Columbia" 
                             | county == "Asotin" | county == "Garfield" 
                             | county == "Grant" | county =="Whitman" | county == "Spokane" 
                             | county == "Lincoln" | county == "Adams" )

iPNW_loss_climate <- rbind(iPNW_loss_climate, Franklin)






iPNW_loss_climate_2015 <- subset(iPNW_loss_climate, year == var_year)

#tmmx

iPNW_loss_climate_2015_tmmx <- iPNW_loss_climate_2015[order(iPNW_loss_climate_2015$tmmx),] 


yrangemin <- min(eval(parse(text=paste("iPNW_loss_climate_2015_tmmx$", unit, sep="")))) - (.05 * min(eval(parse(text=paste("iPNW_loss_climate_2015_tmmx$", unit, sep="")))))
yrangemax <- max(eval(parse(text=paste("iPNW_loss_climate_2015_tmmx$", unit, sep="")))) + (.05 * max(eval(parse(text=paste("iPNW_loss_climate_2015_tmmx$", unit, sep="")))))

y2rangemin <- min(iPNW_loss_climate_2015_tmmx$tmmx) - (.05 * min(iPNW_loss_climate_2015_tmmx$tmmx))
y2rangemax <- max(iPNW_loss_climate_2015_tmmx$tmmx) + (.05 * max(iPNW_loss_climate_2015_tmmx$tmmx))

twoord.plot(c(1:nrow(iPNW_loss_climate_2015_tmmx)), eval(parse(text=paste("iPNW_loss_climate_2015_tmmx$", unit, sep=""))), c(1:nrow(iPNW_loss_climate_2015_tmmx)), rylim=c(y2rangemin, y2rangemax), lylim=c(yrangemin, yrangemax), iPNW_loss_climate_2015_tmmx$tmmx, mar=c(8,4,4,4), ylab = "Wheat insurance loss per acre, due to drought ($)", xticklab=c(" "), rylab = "Annual Mean Max Temperature (K)", type=c("bar", "b"), lcol = "red", rcol = "blue", main = paste(var_year, " iPNW wheat/drought insurance loss per acre\n compared to annual mean max temperature(K) ", sep=""))
text(1:nrow(iPNW_loss_climate_2015_tmmx), yrangemin - 1, srt = 90, adj = 1,
     labels = iPNW_loss_climate_2015_tmmx$county, xpd = TRUE)
mtext(1, text = "iPNW counties", line = 5)

#pr


iPNW_loss_climate_2015_pr <- iPNW_loss_climate_2015[order(-iPNW_loss_climate_2015$pr),] 


yrangemin <- min(iPNW_loss_climate_2015_pr$lossperacre) - 10
yrangemax <- max(iPNW_loss_climate_2015_pr$lossperacre) + 10

y2rangemin <- min(iPNW_loss_climate_2015_pr$pr) - (.05 * min(iPNW_loss_climate_2015_pr$pr))
y2rangemax <- max(iPNW_loss_climate_2015_pr$pr) + (.05 * max(iPNW_loss_climate_2015_pr$pr))

twoord.plot(c(1:nrow(iPNW_loss_climate_2015_pr)), iPNW_loss_climate_2015_pr$lossperacre, c(1:nrow(iPNW_loss_climate_2015_pr)), rylim=c(y2rangemin, y2rangemax), lylim=c(yrangemin, yrangemax), iPNW_loss_climate_2015_pr$pr, mar=c(8,4,4,4), ylab = "Wheat insurance loss per acre, due to drought ($)", xticklab=c(" "), rylab = "Annual Mean Precipitation (mm)", type=c("bar", "b"), lcol = "red", rcol = "blue", main = paste(var_year, " iPNW wheat/drought insurance loss per acre\n compared to annual mean precipitation ", sep=""))
text(1:nrow(iPNW_loss_climate_2015_pr), yrangemin - 1, srt = 90, adj = 1,
     labels = iPNW_loss_climate_2015_pr$county, xpd = TRUE)
mtext(1, text = "iPNW counties", line = 5)


#pet


iPNW_loss_climate_2015_pet <- iPNW_loss_climate_2015[order(iPNW_loss_climate_2015$pet),] 


yrangemin <- min(iPNW_loss_climate_2015_pet$lossperacre) - 10
yrangemax <- max(iPNW_loss_climate_2015_pet$lossperacre) + 10

y2rangemin <- min(iPNW_loss_climate_2015_pet$pet) - (.05 * min(iPNW_loss_climate_2015_pet$pet))
y2rangemax <- max(iPNW_loss_climate_2015_pet$pet) + (.05 * max(iPNW_loss_climate_2015_pet$pet))

twoord.plot(c(1:nrow(iPNW_loss_climate_2015_pet)), iPNW_loss_climate_2015_pet$lossperacre, c(1:nrow(iPNW_loss_climate_2015_pet)), rylim=c(y2rangemin, y2rangemax), lylim=c(yrangemin, yrangemax), iPNW_loss_climate_2015_pet$pet, mar=c(8,4,4,4), ylab = "Wheat insurance loss per acre, due to drought ($)", xticklab=c(" "), rylab = "Annual Mean PET (mm)", type=c("bar", "b"), lcol = "red", rcol = "blue", main = paste(var_year, " iPNW wheat/drought insurance loss per acre\n compared to annual mean PET (mm) ", sep=""))
text(1:nrow(iPNW_loss_climate_2015_pet), yrangemin - 1, srt = 90, adj = 1,
     labels = iPNW_loss_climate_2015_pet$county, xpd = TRUE)
mtext(1, text = "iPNW counties", line = 5)




}

