climatematrix <- function(state_var, county_var, commodity_var, damage_var, climate_var, predictor_var) {
  state_var <- "WA"
  county_var <- "Whitman"
  commodity_var <- "WHEAT"
  damage_var <- "Drought"
  climate_var <- "pet"
  predictor_var <- "log_loss"
  
  library(gplots)
  library(plyr)
  library(dplyr)
  #library(plotly)
  #packageVersion('plotly')
  
  #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss
  
  #monthlist is jan-dec, each repeated 12 times 
  monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))  
  #numlist is 12 months, repeated 12 times
  numlist <- as.data.frame(rep((1:12), times = 12))
  #monthnumlist puts month names and 1-12 together, which results in a vector of 144 that is the matrix of 12 by 12
  monthnumlist <- as.data.frame(cbind(monthlist, numlist))
  #renaming columns
  colnames(monthnumlist) <- c("month", "monthcode")
  #put the monthlist all together in one vector
  monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
  #rename the vector
  climmonth <- monthnumlist$combined
  
  designmatrix <- matrix(NA, ncol=12, nrow=12)
  
  #create an empty 144 vector to fill up with correlations between loss and climate
  dmvector <- as.data.frame(rep(NA, times=144))
  
  cl = 0
  for (ppp in climmonth) {
    cl = cl +1
    
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_rev4")
    file1 <- read.csv(paste(state_var, "_", county_var, "_", commodity_var, "_", damage_var, "_", ppp, ".csv", sep=""))
    climatevar <- as.data.frame(cbind((file1[climate_var]), file1[2]))
    
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries_rev3")
    file2 <- as.data.frame(read.csv(paste(state_var, "_", county_var, "_", commodity_var, "_", damage_var, "_", predictor_var, ".csv", sep="")))
    file2 <- subset(file2, state == state_var)
    file2 <- subset(file2, county == county_var)
    colnames(file2) <- c("X", "year", "damagecause", "county", "state", "commodity", predictor_var )
    
    
    climatevar$zscore <- scale(climatevar[1], center = TRUE, scale = TRUE)
    
    #file2$loss <- scale(file2[7], center = TRUE, scale = TRUE)
    colnames(climatevar[3]) <- "zscore"
    kor <- join(climatevar, file2, by = "year")
    kor2 <- subset(kor, damagecause != "NA")
    colnames(kor2)[3] <- "zscore"
    # kor2[9] <- as.numeric(kor2$zscore)
    kor3 <- cor(kor2[1], kor2[9])
    
    #insert kor3 into designmatrix iteratively
    
    dmvector[cl,] <- kor3
    
  }
  
  dmvector <- as.data.frame(dmvector)
  colnames(dmvector) <- "correlations"
  dmvector2 <- (matrix(dmvector$correlations, 12, 12, TRUE) ) 
  dmvector2 <- dmvector2[nrow(dmvector2):1, ]
  dmvector3 <- dmvector2[4:12,]
  
  dmvector3[9,4:12] <- NA
  dmvector3[8,5:12] <- NA
  dmvector3[7,6:12] <- NA
  dmvector3[6,7:12] <- NA
  dmvector3[5,8:12] <- NA
  dmvector3[4,9:12] <- NA
  dmvector3[3,10:12] <- NA
  dmvector3[2,11:12] <- NA
  dmvector3[1,12:12] <- NA
  
  dmvector <- c(dmvector3[9,], dmvector3[8,], dmvector3[7,], dmvector3[6,], dmvector3[5,], dmvector3[4,], dmvector3[3,], dmvector3[2,], dmvector3[1,])
dmvector <- data.frame(dmvector)

  if(climate_var=='pr'){
    dmv <- which.min( dmvector[1:108,1]) 
  } else {
    if(climate_var=='rmin'){
      dmv <- which.min( dmvector[1:108,1]) 
    } else {
      if(climate_var=='rmax'){
        dmv <- which.min( dmvector[1:108,1]) 
      } else {
        if(climate_var=='tmmn'){
          dmv <- which.max( dmvector[1:108,1]) 
        } else {
          if(climate_var=='pet'){
            dmv <- which.max( dmvector[1:108,1]) 
          } else {
            if(climate_var=='fm100'){
              dmv <- which.min( dmvector[1:108,1]) 
            } else {
              if(climate_var=='fm1000'){
                dmv <- which.min( dmvector[1:108,1]) 
              } else {
                if(climate_var=='pdsi'){
                  dmv <- which.min( dmvector[1:108,1]) 
                } else {
                  if(climate_var=='tmmx'){
                    dmv <- which.max( dmvector[1:108,1]) 
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  #dmv <- which.max(abs( dmvector[1:108,1]) )
  dmv <- as.data.frame(dmv)
  colnames(dmv)[1] <- "row"
  
  #dmvector1a <- max(dmvector$correlations)
  #dmvector1b <- data.frame(which=dmvector, correlations=dmvector[dmvector1a, ])
  
  monthnumlist2 <- cbind(as.data.frame(c(1:144)), monthnumlist)
  colnames(monthnumlist2)[1] <- "row"
  
  monthnumlist3 <- subset(monthnumlist2, row == dmv$row)
  
  
  
  #makeRects <- function(tfMat){
  require(utils)
  cAbove <- expand.grid(1:12, 1:9)[monthnumlist3$row,]
  
  monthzzz <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 108)
  
  setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_pngs")
  
  #  png(filename=paste(state1, "_", county1, "_", damage1, "_", climate_variable12, "_designmatrix.png", sep=""))
  lblcol <- c(9:1)
  dmvector3a <- round(dmvector3, digits = 2)
  
  
  newone <<- as.data.frame(expand.grid(1:12, 1:9)[monthnumlist3$row,])
  
  

  
  #dmvector3a[9,4:12] <- NA
  #dmvector3a[8,5:12] <- NA
  #dmvector3a[7,6:12] <- NA
  #dmvector3a[6,7:12] <- NA
  #dmvector3a[5,8:12] <- NA
  #dmvector3a[4,9:12] <- NA
  #dmvector3a[3,10:12] <- NA
  #dmvector3a[2,11:12] <- NA
  #dmvector3a[1,12:12] <- NA

  
  my_palette <- colorRampPalette(c("lightyellow", "yellow", "orange", "darkorange", "red", "darkred"))(n = 32)
  
  nba_heatmap <- heatmap.2(dmvector3, Rowv=NA, Colv=NA, cexCol = 2, cexRow = 2, colsep=1:ncol(dmvector3), rowsep=1:nrow(dmvector3), col=my_palette, symbreaks = FALSE, scale="none", dendrogram = "none", trace="none", labRow = rev(monthzzz), tracecol = "black", key = TRUE, notecol = "black", notecex = 1.5, key.ylab = "", key.xlab = "loss correlation range", key.title = "", main=paste(climate_var, " vs ", predictor_var, "\n", county_var, " County ", state_var, " ", damage_var, "\n", "MAX Correlation in cell: ",  monthnumlist3$combined, sep=""))
  
  dmvector3
}
