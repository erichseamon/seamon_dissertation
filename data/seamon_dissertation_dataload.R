#-seamon_dissertation_dataload.R
#this R script downloads data from github and places all data in the /tmp directory.
#appendices then access this data from this /tmp directory
#
#author: Erich Seamon
#09/29/2019

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climatology/climatology.zip"
destfile <- "/tmp/seamon/climatology.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/climatology/"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/RMA_originaldata/RMA_originaldata_txt.zip"
destfile <- "/tmp/seamon/RMA_originaldata_txt.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/RMA_originaldata/"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climate_matrices/climate_matrices.zip"
destfile <- "/tmp/seamon/climate_matrices.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/climate_matrices"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climate_correlation_summaries/climate_correlations_summaries.zip"
destfile <- "/tmp/seamon/climate_correlations_summaries.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/climate_correlations_summaries"
unzip(destfile,exdir=outDir) 

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climate_correlations/climate_correlations.zip"
destfile <- "/tmp/seamon/climate_correlations.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/climate_correlations"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climate_outputs/climate_outputs.zip"
destfile <- "/tmp/seamon/climate_outputs.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/climate_outputs"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/states/states_conus.zip"
destfile <- "/tmp/seamon/states_conus.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/states_conus"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/states/threestate_boundary.zip"
destfile <- "/tmp/seamon/threestate_boundary.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/threestate_boundary"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/counties/threestate_willamette.zip"
destfile <- "/tmp/seamon/threestate_willamette.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/threestate_willamette"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/counties/threestate_southernID.zip"
destfile <- "/tmp/seamon/threestate_southernID.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/threestate_southernID"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/counties/threestate_palouse.zip"
destfile <- "/tmp/seamon/threestate_palouse.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/threestate_palouse"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/CPI/CPI.zip"
destfile <- "/tmp/seamon/CPI.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/CPI"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/wheatproduction/wheatyields.zip"
destfile <- "/tmp/seamon/wheatyields.zip"
download.file(URL, destfile)
outDir<-"/tmp/seamon/wheatyields"
unzip(destfile,exdir=outDir)





countiesfips <- read.csv(text=getURL
                         ("https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/counties/counties_fips.csv"), 
                         header = TRUE, strip.white = TRUE, sep = ",")

colnames(countiesfips) <- c("countyfips", "county", "state")

write.csv(countiesfips, file = "/tmp/seamon/countiesfips.csv")



wheatprice <- read.csv(text=RCurl::getURL
                       ("https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/wheat_prices/wheat_prices_monthly_1998_2017.csv"), header = TRUE, strip.white = TRUE, sep = ",")
#wheatprice <- wheatprice[-1]

colnames(wheatprice) <- c("year", "month", "price")

write.csv(wheatprice, file = "/tmp/seamon/wheatprice.csv")



wheatproduction <- read.csv(text=getURL
                            ("https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/wheatproduction/wheatproduction_WA_ID_OR_2001_2015.csv"), 
                            header = TRUE, strip.white = TRUE, sep = ",")

colnames(wheatproduction) <- c("Year", "State", "Value")

write.csv(wheatproduction, file = "/tmp/seamon/wheatproduction.csv")



