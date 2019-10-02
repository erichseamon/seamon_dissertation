#-seamon_dissertation_dataload.R
#this R script downloads data from github and places all data in the /tmp directory.
#appendices then access this data from this /tmp directory
#
#author: Erich Seamon
#09/29/2019

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climatology/climatology.zip"
destfile <- "/tmp/climatology.zip"
download.file(URL, destfile)
outDir<-"/tmp/climatology/"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/RMA_originaldata/RMA_originaldata_txt.zip"
destfile <- "/tmp/RMA_originaldata_txt.zip"
download.file(URL, destfile)
outDir<-"/tmp/RMA_originaldata/"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climate_matrices/climate_matrices.zip"
destfile <- "/tmp/climate_matrices.zip"
download.file(URL, destfile)
outDir<-"/tmp/climate_matrices"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climate_correlation_summaries/climate_correlations_summaries.zip"
destfile <- "/tmp/climate_correlations_summaries.zip"
download.file(URL, destfile)
outDir<-"/tmp/climate_correlations_summaries"
unzip(destfile,exdir=outDir) 

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climate_correlations/climate_correlations.zip"
destfile <- "/tmp/climate_correlations.zip"
download.file(URL, destfile)
outDir<-"/tmp/climate_correlations"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/climate_outputs/climate_outputs.zip"
destfile <- "/tmp/climate_outputs.zip"
download.file(URL, destfile)
outDir<-"/tmp/climate_outputs"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/states/states_conus.zip"
destfile <- "/tmp/states_conus.zip"
download.file(URL, destfile)
outDir<-"/tmp"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/states/threestate_boundary.zip"
destfile <- "/tmp/threestate_boundary.zip"
download.file(URL, destfile)
outDir<-"/tmp"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/counties/threestate_willamette.zip"
destfile <- "/tmp/threestate_willamette.zip"
download.file(URL, destfile)
outDir<-"/tmp"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/counties/threestate_southernID.zip"
destfile <- "/tmp/threestate_southernID.zip"
download.file(URL, destfile)
outDir<-"/tmp"
unzip(destfile,exdir=outDir)

URL <- "https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/counties/threestate_palouse.zip"
destfile <- "/tmp/threestate_palouse.zip"
download.file(URL, destfile)
outDir<-"/tmp"
unzip(destfile,exdir=outDir)





countiesfips <- read.csv(text=getURL
                         ("https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/counties/counties_fips.csv"), 
                         header = TRUE, strip.white = TRUE, sep = ",")

colnames(countiesfips) <- c("countyfips", "county", "state")

write.csv(countiesfips, file = "/tmp/countiesfips.csv")



wheatprice <- read.csv(text=RCurl::getURL
                       ("https://raw.githubusercontent.com/erichseamon/seamon_dissertation/master/data/wheat_prices/wheat_prices_1989_2015.csv"), header = TRUE, strip.white = TRUE, sep = ",")
wheatprice <- wheatprice[-1]

colnames(wheatprice) <- c("year", "price")

write.csv(wheatprice, file = "/tmp/wheatprice.csv")



