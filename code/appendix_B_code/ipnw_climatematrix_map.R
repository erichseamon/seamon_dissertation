#climatematrix_map <- function(climate_var, predictor_var) {
  climate_var <- "pdsi"
  predictor_var <- "cube_root_loss"
  #monthend <- "jan"
  #monthnumber <- 2
  
  
  addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                      "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                    opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                    title = NULL, className = "info legend", layerId = NULL, 
                                    group = NULL, data = getMapData(map), decreasing = FALSE) {
    position <- match.arg(position)
    type <- "unknown"
    na.color <- NULL
    extra <- NULL
    if (!missing(pal)) {
      if (!missing(colors)) 
        stop("You must provide either 'pal' or 'colors' (not both)")
      if (missing(title) && inherits(values, "formula")) 
        title <- deparse(values[[2]])
      values <- evalFormula(values, data)
      type <- attr(pal, "colorType", exact = TRUE)
      args <- attr(pal, "colorArgs", exact = TRUE)
      na.color <- args$na.color
      if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
          0) {
        na.color <- NULL
      }
      if (type != "numeric" && !missing(bins)) 
        warning("'bins' is ignored because the palette type is not numeric")
      if (type == "numeric") {
        cuts <- if (length(bins) == 1) 
          pretty(values, bins)
        else bins	
        
        if (length(bins) > 2) 
          if (!all(abs(diff(bins, differences = 2)) <= 
                   sqrt(.Machine$double.eps))) 
            stop("The vector of breaks 'bins' must be equally spaced")
        n <- length(cuts)
        r <- range(values, na.rm = TRUE)
        cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
        n <- length(cuts)
        p <- (cuts - r[1])/(r[2] - r[1])
        extra <- list(p_1 = p[1], p_n = p[n])
        p <- c("", paste0(100 * p, "%"), "")
        if (decreasing == TRUE){
          colors <- pal(rev(c(r[1], cuts, r[2])))
          labels <- rev(labFormat(type = "numeric", cuts))
        }else{
          colors <- pal(c(r[1], cuts, r[2]))
          labels <- rev(labFormat(type = "numeric", cuts))
        }
        colors <- paste(colors, p, sep = " ", collapse = ", ")
        
      }
      else if (type == "bin") {
        cuts <- args$bins
        n <- length(cuts)
        mids <- (cuts[-1] + cuts[-n])/2
        if (decreasing == TRUE){
          colors <- pal(rev(mids))
          labels <- rev(labFormat(type = "bin", cuts))
        }else{
          colors <- pal(mids)
          labels <- labFormat(type = "bin", cuts)
        }
        
      }
      else if (type == "quantile") {
        p <- args$probs
        n <- length(p)
        cuts <- quantile(values, probs = p, na.rm = TRUE)
        mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                         na.rm = TRUE)
        if (decreasing == TRUE){
          colors <- pal(rev(mids))
          labels <- rev(labFormat(type = "quantile", cuts, p))
        }else{
          colors <- pal(mids)
          labels <- labFormat(type = "quantile", cuts, p)
        }
      }
      else if (type == "factor") {
        v <- sort(unique(na.omit(values)))
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
        if (decreasing == TRUE){
          colors <- pal(rev(v))
          labels <- rev(labFormat(type = "factor", v))
        }else{
          colors <- pal(v)
          labels <- labFormat(type = "factor", v)
        }
      }
      else stop("Palette function not supported")
      if (!any(is.na(values))) 
        na.color <- NULL
    }
    else {
      if (length(colors) != length(labels)) 
        stop("'colors' and 'labels' must be of the same length")
    }
    legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                   na_color = na.color, na_label = na.label, opacity = opacity, 
                   position = position, type = type, title = title, extra = extra, 
                   layerId = layerId, className = className, group = group)
    invokeMethod(map, data, "addLegend", legend)
  }
    
      library(RColorBrewer)
      library(dplyr)
      library(ggplot2)
      library(maptools)
  library(leaflet)
      
      setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_correlations_5/")
      
      
      if (predictor_var  == "loss") {
        
        predictor <- "crop_commodity_loss"
        
      }else {
        
        predictor <- predictor_var
      }
      
      
      
      files  <- list.files(pattern = predictor)
      filey <- do.call(rbind, strsplit(files, '[_]'))
      
      filey <- subset(filey, filey[,5] == climate_var)
      
      colnames(filey) <- c("state", "county", "commodity", "damage", "climate", "crop1", "crop2", "response", "crop3")
      filey <- as.data.frame(filey)
      data <- with(filey, paste(state, "_", county, "_", commodity, "_", damage, "_", climate, "_", crop1, "_", crop2, "_", response, "_", crop3, sep=""))
      
      
      
      
      
      
      tables <- lapply(data, read.csv, header = TRUE)
      
      
      
      tables <- lapply(tables, function(x) { x["X"] <- NULL; x }) #--remove first index row from each list
      
      tables <- lapply(tables, function(x) dplyr::arrange(x, -row_number())) #--(flips matrix - puts jan as 1st row and sept as 9th row)
      
      
      for (i in 1:24) {
      tables[[i]][1,4:12] <- NA
      tables[[i]][2,5:12] <- NA
      tables[[i]][3,6:12] <- NA
      tables[[i]][4,7:12] <- NA
      tables[[i]][5,8:12] <- NA
      tables[[i]][6,9:12] <- NA
      tables[[i]][7,10:12] <- NA
      tables[[i]][8,11:12] <- NA
      tables[[i]][9,12:12] <- NA
      }
      
      
      #monthly <- match(monthend, tolower(month.abb))
      
      
      if(climate_var=='pr'){
        
        bestcounty <- matrix(NA,nrow=24,ncol=3)
        for (i in 1:24) {
          temp <- which(tables[[i]] == min(tables[[i]], na.rm=T), arr.ind = TRUE)
          temp2 <- min(tables[[i]], na.rm=T)
          bestcounty[i,1] <- temp[1,1]
          bestcounty[i,2] <- temp[1,2]
          bestcounty[i,3] <- temp2
          temp <- NA
          temp2 <- NA
          
        }
      } else {
        if(climate_var=='rmin'){
          
          bestcounty <- matrix(NA,nrow=24,ncol=3)
          for (i in 1:24) {
            temp <- which(tables[[i]] == min(tables[[i]], na.rm=T), arr.ind = TRUE)
            temp2 <- min(tables[[i]], na.rm=T)
            bestcounty[i,1] <- temp[1,1]
            bestcounty[i,2] <- temp[1,2]
            bestcounty[i,3] <- temp2
            temp <- NA
            temp2 <- NA
          }
        } else {
          if(climate_var=='rmax'){
            
            bestcounty <- matrix(NA,nrow=24,ncol=3)
            for (i in 1:24) {
              temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
              temp2 <- max(tables[[i]], na.rm=T)
              bestcounty[i,1] <- temp[1,1]
              bestcounty[i,2] <- temp[1,2]
              bestcounty[i,3] <- temp2
              temp <- NA
              temp2 <- NA
            }
          } else {  
            if(climate_var=='tmmx'){ 
              bestcounty <- matrix(NA,nrow=24,ncol=3)
              for (i in 1:24) {
                temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                temp2 <- max(tables[[i]], na.rm=T)
                bestcounty[i,1] <- temp[1,1]
                bestcounty[i,2] <- temp[1,2]
                bestcounty[i,3] <- temp2
                temp <- NA
                temp2 <- NA
              }
            } else {
              if(climate_var=='tmin'){
                bestcounty <- matrix(NA,nrow=24,ncol=3)
                for (i in 1:24) {
                  temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                  temp2 <- max(tables[[i]], na.rm=T)
                  bestcounty[i,1] <- temp[1,1]
                  bestcounty[i,2] <- temp[1,2]
                  bestcounty[i,3] <- temp2
                  temp <- NA
                  temp2 <- NA
                }
              } else {
                if(climate_var=='fm100'){
                  bestcounty <- matrix(NA,nrow=24,ncol=3)
                  for (i in 1:24) {
                    temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                    temp2 <- max(tables[[i]], na.rm=T)
                    bestcounty[i,1] <- temp[1,1]
                    bestcounty[i,2] <- temp[1,2]
                    bestcounty[i,3] <- temp2
                    temp <- NA
                    temp2 <- NA
                  }
                } else {
                  if(climate_var=='fm1000'){
                    bestcounty <- matrix(NA,nrow=24,ncol=3)
                    for (i in 1:24) {
                      temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                      temp2 <- max(tables[[i]], na.rm=T)
                      bestcounty[i,1] <- temp[1,1]
                      bestcounty[i,2] <- temp[1,2]
                      bestcounty[i,3] <- temp2
                      temp <- NA
                      temp2 <- NA
                    }
                  } else {
                    if(climate_var=='pet'){
                      bestcounty <- matrix(NA,nrow=24,ncol=3)
                      for (i in 1:24) {
                        temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                        temp2 <- max(tables[[i]], na.rm=T)
                        bestcounty[i,1] <- temp[1,1]
                        bestcounty[i,2] <- temp[1,2]
                        bestcounty[i,3] <- temp2
                        temp <- NA
                        temp2 <- NA
                      }
                    } else {
                      if(climate_var=='pdsi'){
                        bestcounty <- matrix(NA,nrow=24,ncol=3)
                        for (i in 1:24) {
                          temp <- which(tables[[i]] == min(tables[[i]], na.rm=T), arr.ind = TRUE)
                          temp2 <- min(tables[[i]], na.rm=T)
                          bestcounty[i,1] <- temp[1,1]
                          bestcounty[i,2] <- temp[1,2]
                          bestcounty[i,3] <- temp2
                          temp <- NA
                          temp2 <- NA
                        }
                        
                        
                        
                        
                        
                        
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      
      
      
      bestcounty[,1] <- tolower(month.abb[bestcounty[,1]])
      
      bestcounty2 <- cbind(data.frame(filey$county), bestcounty)
      colnames(bestcounty2) <- c("NAME", "MONTH", "ENDMONTH", "CORRELATION")
      #new
      
      
      
      
      
      #!!!!!!fix-row by column, or number of months by ending month
      table2 <- lapply(tables, function(x) x[monthly, as.numeric(monthnumber)])
      
      
      table3 <- data.frame(matrix(unlist(table2), nrow=length(table2), byrow=T))
      colnames(table3) <- "correlations"
      #combined <- do.call(rbind , tables)
      
      table4 <- cbind(filey, table3)
      
      #if (predictor_var  == "loss") {
      
      #predictor_var <- "crop_commodity_loss"
      
      #}
      
      table5 <- table4[c(2:5,10)]
      
      colnames(table5) <- c("NAME", "COMMODITY", "DAMAGE", "climate", "correlations")
      
      #table5$STATE_NAME <-  state.name[match(table5[,1],state.abb)]
      
      
      
      
      
      setwd("/dmine/data/counties/")
      
      counties <- readShapePoly('UScounties.shp', 
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      statez = c("Idaho", "Washington", "Oregon")
      Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Latah", "Benewah", sep="|")
      Washington_list1 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
      Oregon_list1 <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")
      
      
      combinedlist2 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
      combinedlist <- c(Idaho_list1, Washington_list1, Oregon_list1)
      
      #alllist <- c("Idaho", "Oregon", "Washington")
      
      
      #--Oregon
      
      setwd("/dmine/data/counties/")
      
      counties <- readShapePoly('UScounties.shp', 
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      or_counties <- counties[grep("Oregon", counties@data$STATE_NAME),]
      palouse_Oregon_counties <- or_counties[grep(Oregon_list1, or_counties@data$NAME),]
      kk="Oregon"
      
      #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
      OR_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Oregon_counties)
      #counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
      
      #--loop list for county by fip
      #countyfiploop <- counties@data$FIPS
      
      #--data frame of county fip list
      #countyfiplist <- data.frame(counties@data$FIPS)
      
      #--data frame of county names
      #countynames <- data.frame(counties@data$NAME)
      
      #combo of county names and fip for this list
      #countylist <- cbind(countynames, countyfiplist)
      
      #--number of rows in county list
      #countylistrows <- 12 * nrow(countylist)
      
      
      
      #---Washington
      
      
      
      setwd("/dmine/data/counties/")
      
      counties <- readShapePoly('UScounties.shp', 
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      wa_counties <- counties[grep("Washington", counties@data$STATE_NAME),]
      palouse_Washington_counties <- wa_counties[grep(Washington_list1, wa_counties@data$NAME),]
      kk="Washington"
      
      #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
      WA_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Washington_counties)
      #counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
      
      #-----Idaho
      
      
      setwd("/dmine/data/counties/")
      
      counties <- readShapePoly('UScounties.shp', 
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      id_counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
      palouse_Idaho_counties <- id_counties[grep(Idaho_list1, id_counties@data$NAME),]
      kk="Idaho"
      #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
      ID_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Idaho_counties)
      #counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
      
      counties <- rbind(ID_counties, WA_counties, OR_counties)
      
      
      
      
      counties2 <- merge(counties, table5, by = "NAME" )
      
      #--new
      
      counties3 <- merge(counties2, bestcounty2, by = "NAME")
      counties3$MONTHCOMBO <- paste(counties3$MONTH, counties3$ENDMONTH, sep="")
      
      #--new
      
      
      
      #colorbrew <- list(color = brewer.pal(24, c("green", "blue", "yellow")))
      #my_palette <- colorRampPalette(c("darkred", "lightyellow"))(n = 24)
      
      counties3$CORRELATION <- as.numeric(levels(counties3$CORRELATION))[counties3$CORRELATION]
      
      #pal <- colorNumeric(rev(my_palette),  na.color = "#ffffff", domain = eval(parse(text=paste("counties3$", "CORRELATION", sep=""))))
      
      #--
      
      #colorss = colorRampPalette(brewer.pal(11,"Spectral"))
      
      #finalcol <- colorss(len <- length(counties3$CORRELATION))
      #finalcol2 <- topo.colors(length(counties3$CORRELATION))[order(order(counties3$CORRELATION))]
      
      #cellselect <- paste(monthend, monthnumber, sep="")
      
      #par(mfrow=c(1,4))
      #layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE)) 
      #par(mar = c(1,1,1,1) + 0.1)
      #plot(counties3, col = finalcol2, xaxs="i", yaxs="i")
      ##text(coordinates(counties2), labels=round(counties2$correlations, 2), cex=1.5, col = "black")
      
      #added from ID
      #corre <- round(as.numeric(as.character(counties3$CORRELATION)), 2)
      #text(coordinates(counties2), labels=paste(counties3$MONTHCOMBO, "\n", corre,  sep=""), cex=1.5, col = "white", font = 2)
      
      #--
      
      exte <- extent(counties3)
      
      library(htmltools)
      
      tag.map.title <- tags$style(HTML("
                                       .leaflet-control.map-title { 
                                       transform: translate(-50%,20%);
                                       position: fixed !important;
                                       left: 50%;
                                       text-align: center;
                                       padding-left: 10px; 
                                       padding-right: 10px; 
                                       background: rgba(255,255,255,0.75);
                                       font-weight: bold;
                                       font-size: 24px;
                                       }
                                       "))
      
      title <- tags$div(
        tag.map.title, HTML(paste("IPNW  Correlation, Climate vs. ", predictor_var, " by County for ", climate_var, sep=""))
      )  

      
      lat_long <- coordinates(counties3)
      
     
      #labels <- paste(counties3$MONTHCOMBO, as.character(round(counties3$CORRELATION, 2)), sep = "<br/>")
   
      
      #counties3$CORRELATION <- as.numeric(levels(counties3$CORRELATION))[counties3$CORRELATION]
      counties3a <- data.frame(counties3)
      labs <- lapply(seq(nrow(counties3a)), function(i) {
        paste0(as.character(round(counties3a[i, "CORRELATION"],2)), '<br/>',
                counties3a[i, "MONTHCOMBO"]) 
      })

      
      if (climate_var == "pr") {
        
        pal <- colorNumeric(colorRampPalette(c("red", "orange", "yellow", "lightyellow"))(11), na.color = "#ffffff",
                            domain = eval(parse(text=paste("counties3$", "CORRELATION", sep=""))))
        
        palrev <- colorNumeric(colorRampPalette(c("lightyellow", "yellow", "orange", "red"))(11), na.color = "#ffffff",
                               domain = eval(parse(text=paste("counties3$", "CORRELATION", sep=""))))
        
        
        map <- leaflet(data = counties3, options = leafletOptions(zoomControl = FALSE)) %>% 
          
          addProviderTiles(providers$Hydda.Base) %>%
          addProviderTiles(providers$Stamen.TonerLines) %>%
          
          #addControl(title, position = "topleft", className="map-title") %>% 
          
          fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = ~pal(counties3$CORRELATION), fillOpacity = .8, weight = 2, stroke = TRUE, smoothFactor = 0.5, color = "black") %>%
          addLabelOnlyMarkers(data = counties3, lng = lat_long[,1], lat = lat_long[,2], label = lapply(labs, HTML), labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE, textsize = "14px", col = "white", style = list(
            color = ~pal))) %>%
          
          addLegend_decreasing(pal = palrev, values = counties3$CORRELATION,  labels = c("1", "2"), opacity = .5, title = paste("Correlation",  " Matrix", sep="<br>"),
                               position = "bottomright")
        
        map
        
        
        } else if (climate_var == "pdsi") {
          
          pal <- colorNumeric(colorRampPalette(c("red", "orange", "yellow", "lightyellow"))(11), na.color = "#ffffff",
                              domain = eval(parse(text=paste("counties3$", "CORRELATION", sep=""))))
          
          palrev <- colorNumeric(colorRampPalette(c("lightyellow", "yellow", "orange", "red"))(11), na.color = "#ffffff",
                                 domain = eval(parse(text=paste("counties3$", "CORRELATION", sep=""))))
          
          
          map <- leaflet(data = counties3, options = leafletOptions(zoomControl = FALSE)) %>% 
            
            addProviderTiles(providers$Hydda.Base) %>%
            addProviderTiles(providers$Stamen.TonerLines) %>%
            
            #addControl(title, position = "topleft", className="map-title") %>% 
            
            fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = ~pal(counties3$CORRELATION), fillOpacity = .8, weight = 2, stroke = TRUE, smoothFactor = 0.5, color = "black") %>%
            addLabelOnlyMarkers(data = counties3, lng = lat_long[,1], lat = lat_long[,2], label = lapply(labs, HTML), labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE, textsize = "14px", col = "white")) %>%
            
            addLegend_decreasing(pal = palrev, values = counties3$CORRELATION,  labels = c("1", "2"), opacity = .5, title = paste("Correlation",  " Matrix", sep="<br>"),
                                 position = "bottomright")
          
          map
          
          
          
      } else {
        pal <- colorNumeric(colorRampPalette(c("lightyellow", "yellow", "orange", "red"))(11), na.color = "#ffffff",
                            domain = eval(parse(text=paste("counties3$", "CORRELATION", sep=""))))
        
        
      map <- leaflet(data = counties3, options = leafletOptions(zoomControl = FALSE)) %>% 
        
        addProviderTiles(providers$Hydda.Base) %>%
        addProviderTiles(providers$Stamen.TonerLines) %>%
        
        #addControl(title, position = "topleft", className="map-title") %>% 
        
        fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = ~pal(counties3$CORRELATION), fillOpacity = .8, weight = 2, stroke = TRUE, smoothFactor = 0.5, color = "black") %>%
        addLabelOnlyMarkers(data = counties3, lng = lat_long[,1], lat = lat_long[,2], label = lapply(labs, HTML), labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE, textsize = "14px", col = "white")) %>%
        
        addLegend(pal = pal, values = counties3$CORRELATION,  labels = c("1", "2"), opacity = .5, title = paste("Correlation",  " Matrix", sep="<br>"),
                  position = "bottomright")
      
      map
      
      }
      
      #---
   
     
      counties3a$MONTH <- as.character(levels(counties3a$MONTH))[counties3a$MONTH]
      counties3a$ENDMONTH <- as.numeric(levels(counties3a$ENDMONTH))[counties3a$ENDMONTH]
      
      
      capFirst <- function(s) {
        paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
      }
      
      counties_month <- capFirst(counties3a$MONTH)
      counties_month <- match(counties_month,month.abb)
      w <- cbind(data.frame(counties3a$STATE_NAME), data.frame(counties3a$NAME), counties_month + 3, counties3a$ENDMONTH + 3, (counties_month - counties3a$ENDMONTH)+3, counties3$CORRELATION)
      colnames(w) <- c("State", "County", "endmonth", "startmonth1", "startmonth2", "correlation")
      w$ID <- 1:nrow(w)
      w$County <- as.character(w$County)
      
      months <- c("oct", "nov", "dec", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct")
      
      par(mar=c(6, 7, 2, 3), family = 'serif', mgp=c(5, 1, 0), las=0)
      
      
      if (climate_var == "pr") {
      
      p <- ggplot(w) +
        aes(ymin = `startmonth2`,
            ymax = `endmonth`,
            xmin = w$ID - .3,
            xmax = w$ID + .3,
            
            x = reorder(County, ID),
            fill = `correlation`
            
        ) +
        ggtitle(paste("Monthly Time-lagged Correlations for ", climate_var, sep="")) + geom_rect() + theme_light() + theme(axis.title.x = element_blank(),  axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ylab("Months") + xlab("Counties") + scale_y_continuous(labels= months, breaks = c(1:13)) + scale_fill_gradient(guide = guide_legend(reverse=TRUE), low = "red", high = "yellow") + coord_flip()  + theme(text = element_text(size = 24, family = "Serif"))
      
      } else if (climate_var == "pdsi") {
        
        p <- ggplot(w) +
          aes(ymin = `startmonth2`,
              ymax = `endmonth`,
              xmin = w$ID - .3,
              xmax = w$ID + .3,
              
              x = reorder(County, ID),
              fill = `correlation`
              
          ) +
          ggtitle(paste("Monthly Time-lagged Correlations for ", climate_var, sep="")) + geom_rect() + theme_light() + theme(axis.title.x = element_blank(),  axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ylab("Months") + xlab("Counties") + scale_y_continuous(labels= months, breaks = c(1:13)) + scale_fill_gradient(guide = guide_legend(reverse=TRUE), low = "red", high = "yellow") + coord_flip()  + theme(text = element_text(size = 24, family = "Serif"))
      
      } else {
        
        p <- ggplot2::ggplot(w) +
        aes(ymin = `startmonth2`,
            ymax = `endmonth`,
            xmin = w$ID - .3,
            xmax = w$ID + .3,
            
            x = reorder(County, ID),
            fill = `correlation`
            
        ) +   
          
          
        ggtitle(paste("Monthly Time-lagged Correlations for ", climate_var, sep="")) + geom_rect() + theme_light() + theme(axis.title.x = element_blank(),  axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ylab("Months") + xlab("Counties") + scale_y_continuous(labels= months, breaks = c(1:13)) + scale_fill_gradient(guide = guide_legend(reverse=FALSE), low = "yellow", high = "red") + coord_flip() + theme(text = element_text(size = 24, family = "Serif"))  
      
}
        
p 

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_rev3")  

monthcombo <- 1:nrow(counties3a)




empty_df2 <- data.frame(matrix(ncol = 1, nrow = 624))

climlist <- c("pr", "pet", "tmmx", "tmmn", "pdsi", "srad", "rmax", "rmin", "bi", "erc")

for (jj in climlist) {
  
  empty_df <- data.frame(matrix(ncol = 1, nrow = 0))
  x <- c("climate")
  colnames(empty_df) <- x
  
 for (j in monthcombo)   {
   ind <- counties3a[j,]
   state1 <- state.abb[match(ind[,2],state.name)]
   csv <- read.csv(paste(state1, "_", ind$NAME, "_", "WHEAT_Drought_", ind$MONTHCOMBO, ".csv", sep=""))
   climm <- data.frame(eval(parse(text=paste("csv$", jj, sep=""))) )
   countee <- data.frame(csv$year)
   climcountee <- cbind(climm)
   colnames(climcountee) <- c("climate")
   empty_df <- rbind(empty_df, climcountee)

 }
  
  empty_df2 <- cbind(empty_df2, empty_df)
  
}

empty_df_county <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("year", "county", "state")
colnames(empty_df_county) <- x


for (j in monthcombo)   {
  ind <- counties3a[j,]
  state1 <- state.abb[match(ind[,2],state.name)]
  csv <- read.csv(paste(state1, "_", ind$NAME, "_", "WHEAT_Drought_", ind$MONTHCOMBO, ".csv", sep=""))
  climm <- data.frame(eval(parse(text=paste("csv$", jj, sep=""))) )
  countee <- data.frame(csv$year)
  climcountee <- cbind(countee, ind$NAME, ind$STATE_NAME)
  colnames(climcountee) <- c("year", "county", "state")
  empty_df_county <- rbind(empty_df_county, climcountee)
  
}


newclim <- cbind(empty_df2, empty_df_county)



        
        



#---creating files of data for all counties

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

vectorname <- as.vector(bestcounty2$NAME)
vectorname <- vectorname[-2]
vectorname <- vectorname[-3]
statename <- c("ID", "ID", "ID", "ID", "ID", "OR", "OR", "OR", "OR", "OR", "OR", "OR", "WA", "WA", "WA", "WA", "WA", "WA", "WA", "WA", "WA", "WA", "WA", "WA")
vectormonth <- as.vector(bestcounty2$MONTH)
vectorendmonth <- as.vector(bestcounty2$ENDMONTH)
monthendmonth <- paste(vectormonth, vectorendmonth, sep="")

df_total = data.frame()
for (i in 1:24) {

countyname <- vectorname[i]
endmonthname <- monthendmonth[i]

#  for (ppp in climmonth) {
cl = cl +1


setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_rev4")
file1 <- read.csv(paste(statename[i], "_", countyname, "_", "WHEAT", "_", "Drought", "_", endmonthname, ".csv", sep=""))
climatevar <- as.data.frame(cbind((file1[climate_var]), file1[2]))


setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries_rev3")
file2 <- as.data.frame(read.csv(paste(statename[i], "_", countyname, "_", "WHEAT", "_", "Drought", "_", "cube_root_loss", ".csv", sep="")))
file2 <- subset(file2, state == statename[i])
file2 <- subset(file2, county == countyname)
colnames(file2) <- c("X", "year", "damagecause", "county", "state", "commodity", "cube_root_loss")
climatevar$zscore <- scale(climatevar[1], center = TRUE, scale = TRUE)
colnames(climatevar[3]) <- "zscore"
kor <- join(climatevar, file2, by = "year")
kor2 <- subset(kor, damagecause != "NA")
colnames(kor2)[3] <- "zscore"
kor2$monthcombo <- c(endmonthname)
# kor2[9] <- as.numeric(kor2$zscore)
kor3 <- cor(kor2[1], kor2[9])


#insert kor3 into designmatrix iteratively

#   dmvector[cl,] <- kor3
#  }

#  dmvector <- as.data.frame(dmvector)
#  colnames(dmvector) <- "correlations"
#  dmvector2 <- (matrix(dmvector$correlations, 12, 12, TRUE) )
#  dmvector2 <- dmvector2[nrow(dmvector2):1, ]
#  dmvector3 <- dmvector2[4:12,]

#dmvector4 <- data.table(round(dmvector3, digits = 3))

#dmvector4

colzz <- c(1,2,6,7,9,10)
korprint <- data.table(kor2[colzz])
#climatevar4 <- data.table(climatevar2)
df_total <- rbind(df_total,korprint)

}

df_total <- data.frame(df_total)
setwd("/tmp/")
write.csv(df_total, "WHEAT_Drought_tmmx_cube_root_loss.csv")



