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

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries3")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE, strip.white = TRUE)
ziggy.df <- do.call(rbind , myfiles)

ziggy.df$pr <- ziggy.df$pr * 30
ziggy.df$pet <- ziggy.df$pet * 30

#OR1 <- subset(ziggy.df, year == 2008)

OR1 <- aggregate(.~countyfips + month, ziggy.df, mean)

countiesfips <- read.csv("/dmine/data/counties/counties_fips.csv", header = TRUE)
colnames(countiesfips) <- c("countyfips", "county", "state")

OR2 <- merge(countiesfips, OR1, by=("countyfips"))


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries3")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE, strip.white = TRUE)
ziggy.df <- do.call(rbind , myfiles)

ziggy.df$pr <- ziggy.df$pr * 30
ziggy.df$pet <- ziggy.df$pet * 30
#WA1 <- subset(ziggy.df, year == 2008)

WA1 <- aggregate(.~countyfips + month, ziggy.df, mean)

countiesfips <- read.csv("/dmine/data/counties/counties_fips.csv", header = TRUE)
colnames(countiesfips) <- c("countyfips", "county", "state")

WA2 <- merge(countiesfips, WA1, by=("countyfips"))



simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries3")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE, strip.white = TRUE)
ziggy.df <- do.call(rbind , myfiles)

ziggy.df$pr <- ziggy.df$pr * 30
ziggy.df$pet <- ziggy.df$pet * 30

#ID1 <- subset(ziggy.df, year == 2008)

ID1 <- aggregate(.~countyfips + month, ziggy.df, mean)

countiesfips <- read.csv("/dmine/data/counties/counties_fips.csv", header = TRUE)
colnames(countiesfips) <- c("countyfips", "county", "state")

ID2 <- merge(countiesfips, ID1, by=("countyfips"))

ID2 <- subset(ID2, county == "Benewah" 
              | county == "Latah" | county == "Nez Perce" | county == "Lewis" 
              | county == "Idaho")

WA2 <- subset(WA2,county == "Douglas" | county == "Franklin" 
              | county == "Walla Walla" | county == "Benton" | county == "Columbia" 
              | county == "Asotin" | county == "Garfield" 
              | county == "Grant" | county =="Whitman" | county == "Spokane" 
              | county == "Lincoln" | county == "Adams" ) 

OR2 <- subset(OR2, county == "Wasco" | county == "Sherman" 
              | county == "Gilliam" | county == "Morrow" | county == "Umatilla" 
              | county == "Union" | county == "Wallowa")


iPNW2 <- rbind(ID2, OR2, WA2)


#Umatilla_OR <- subset(OR2, county == "Umatilla")

iPNW2$month <- factor(iPNW2$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

#OR2$month <- sapply(as.character(OR2$month), simpleCap)

#Umatilla_OR <- Umatilla_OR[order(match(Umatilla_OR$month, month.abb)), ]

iPNW2$state <- factor(iPNW2$state)
iPNW2$county <- factor(iPNW2$county)



par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
interaction.plot(x.factor     = iPNW2$month,
                 trace.factor = iPNW2$county, 
                 response     = iPNW2$pdsi, 
                 
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19),             ### Symbols for levels of trace var.
                 fixed=FALSE, 
                 legend=F,
                 ### Order by factor order in data
                 leg.bty = "n",
                 ylab="2001 - 2015 Monthly Average County Precipitation (mm)",
                 xlab="Months",
                 main="Interaction Plot - Loss ($) vs. year for Select Counties, 1998-2015", las = 2)


p <- ggplot(data = iPNW2, aes(x = month, y = pdsi, group = county,
                                  colour = state))
p + geom_line() + theme_minimal() + theme(text = element_text(size=20),
                                                       axis.text.x = element_text(angle=90, hjust=1)) +
  ggtitle("") +
  xlab("Months") + ylab("Palmer Drought Severity Index (PDSI)") +
  theme(text=element_text(size=24,  family="Serif"))

ggplot(iPNW2, aes(x = month, y = pr, group = county, color = state)) + 
  geom_point(aes(color = state, group = county), size = 1) +
  theme_minimal() 


#--sum by county to get average 

iPNW3 <- aggregate(iPNW2$pr, by=list(iPNW2$state, iPNW2$county), FUN = "sum")
colnames(iPNW3)<- c("STATE_NAME", "NAME", "annual_precip")

iPNW3$STATE_NAME <- state.name[match(iPNW3$STATE_NAME,state.abb)]

iPNW4 <- merge(counties, iPNW3, by=c("STATE_NAME", "NAME"))

#plot map of Precipitation gradient

label <- paste(sep = "<br/>", iPNW4$annual_precip)
markers <- data.frame(label)
labs <- lapply(seq(nrow(iPNW4)), function(i) {
  paste0(as.character(round(iPNW4@data[i, "annual_precip"],0)), "mm")})

exte <- as.vector(extent(iPNW4))


pal <- colorNumeric(colorRampPalette(c("yellow", "red"))(11), na.color = "#ffffff",
                    domain = eval(parse(text=paste("iPNW4$annual_precip", sep=""))))

map <- leaflet(data = iPNW4, options = leafletOptions(zoomControl = FALSE, zoomSnap = 0.1)) %>%  
  addProviderTiles(providers$Hydda.Base) %>%
  addProviderTiles(providers$Stamen.TonerLines) %>%
  
  addMiniMap(
    tiles = providers$Stamen.TonerLite,
    position = 'topleft', 
    width = 100, height = 100,
    toggleDisplay = FALSE) %>%
  
  
  #addControl(title, position = "topleft", className="map-title") %>%

  
  fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = ~pal(iPNW4$annual_precip),  fillOpacity = .8, weight = 1, stroke = TRUE, smoothFactor = 0.5, color = "black") %>% addPolygons(data = states, fillOpacity = 0, weight = 4, stroke = TRUE, smoothFactor = 0.5, color = "black") %>% 
  
  addLegend(pal = pal, values = na.omit(~annual_precip), group = "circles", title = paste("Annual Precip. Ave.", sep = ""), na.label = "", position = "topright", labFormat = labelFormat(suffix = "mm")) %>%
  
  
  addLabelOnlyMarkers(data = iPNW4, lng = lat_long[,1], lat = lat_long[,2], label = lapply(labs, HTML), labelOptions = labelOptions(style = list("font-family" = "serif"), noHide = TRUE, direction = 'middle', textOnly = TRUE, textsize = "18px", col = "white",  offset = c(20, 0), markerOptions(riseOnHover = TRUE)
  )) 

addScaleBar(map, position = c("topright"), options = scaleBarOptions())




#--sum by county to get average 

iPNW3_pet <- aggregate(iPNW2$pet, by=list(iPNW2$state, iPNW2$county), FUN = "sum")
colnames(iPNW3_pet)<- c("STATE_NAME", "NAME", "annual_pet")

iPNW3_pet$STATE_NAME <- state.name[match(iPNW3_pet$STATE_NAME,state.abb)]

iPNW4_pet <- merge(counties, iPNW3_pet, by=c("STATE_NAME", "NAME"))

#plot map of Precipitation gradient

label <- paste(sep = "<br/>", iPNW4_pet$annual_pet)
markers <- data.frame(label)
labs <- lapply(seq(nrow(iPNW4_pet)), function(i) {
  paste0(as.character(round(iPNW4_pet@data[i, "annual_pet"],0)), "mm")})

exte <- as.vector(extent(iPNW4_pet))


pal <- colorNumeric(colorRampPalette(c("yellow", "red"))(11), na.color = "#ffffff",
                    domain = eval(parse(text=paste("iPNW4_pet$annual_pet", sep=""))))

map <- leaflet(data = iPNW4_pet, options = leafletOptions(zoomControl = FALSE, zoomSnap = 0.1)) %>%  
  addProviderTiles(providers$Hydda.Base) %>%
  addProviderTiles(providers$Stamen.TonerLines) %>%
  
  addMiniMap(
    tiles = providers$Stamen.TonerLite,
    position = 'topleft', 
    width = 100, height = 100,
    toggleDisplay = FALSE) %>%
  
  
  #addControl(title, position = "topleft", className="map-title") %>%
  
  
  fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = ~pal(iPNW4_pet$annual_pet),  fillOpacity = .8, weight = 1, stroke = TRUE, smoothFactor = 0.5, color = "black") %>% addPolygons(data = states, fillOpacity = 0, weight = 4, stroke = TRUE, smoothFactor = 0.5, color = "black") %>% 
  
  addLegend(pal = pal, values = na.omit(~annual_pet), group = "circles", title = paste("Annual PET Ave.", sep = ""), na.label = "", position = "topright", labFormat = labelFormat(suffix = "mm")) %>%
  
  
  addLabelOnlyMarkers(data = iPNW4_pet, lng = lat_long[,1], lat = lat_long[,2], label = lapply(labs, HTML), labelOptions = labelOptions(style = list("font-family" = "serif"), noHide = TRUE, direction = 'middle', textOnly = TRUE, textsize = "18px", col = "white",  offset = c(20, 0), markerOptions(riseOnHover = TRUE)
  )) 

addScaleBar(map, position = c("topright"), options = scaleBarOptions())



#--sum by county to get average 

iPNW3_tmmx <- aggregate(iPNW2$tmmx, by=list(iPNW2$state, iPNW2$county), FUN = "mean")
colnames(iPNW3_tmmx)<- c("STATE_NAME", "NAME", "annual_tmmx")

iPNW3_tmmx$annual_tmmx <- iPNW3_tmmx$annual_tmmx - 273

iPNW3_tmmx$STATE_NAME <- state.name[match(iPNW3_tmmx$STATE_NAME,state.abb)]

iPNW4_tmmx <- merge(counties, iPNW3_tmmx, by=c("STATE_NAME", "NAME"))

#plot map of Precipitation gradient

label <- paste(sep = "<br/>", iPNW4_tmmx$annual_tmmx)
markers <- data.frame(label)
labs <- lapply(seq(nrow(iPNW4_tmmx)), function(i) {
  paste0(as.character(round(iPNW4_tmmx@data[i, "annual_tmmx"],1)), intToUtf8(176), "C")})

exte <- as.vector(extent(iPNW4_tmmx))


pal <- colorNumeric(colorRampPalette(c("yellow", "red"))(11), na.color = "#ffffff",
                    domain = eval(parse(text=paste("iPNW4_tmmx$annual_tmmx", sep=""))))

map <- leaflet(data = iPNW4_tmmx, options = leafletOptions(zoomControl = FALSE, zoomSnap = 0.1)) %>%  
  addProviderTiles(providers$Hydda.Base) %>%
  addProviderTiles(providers$Stamen.TonerLines) %>%
  
  addMiniMap(
    tiles = providers$Stamen.TonerLite,
    position = 'topleft', 
    width = 100, height = 100,
    toggleDisplay = FALSE) %>%
  
  
  #addControl(title, position = "topleft", className="map-title") %>%
  
  
  fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = ~pal(iPNW4_tmmx$annual_tmmx),  fillOpacity = .8, weight = 1, stroke = TRUE, smoothFactor = 0.5, color = "black") %>% addPolygons(data = states, fillOpacity = 0, weight = 4, stroke = TRUE, smoothFactor = 0.5, color = "black") %>% 
  
  addLegend(pal = pal, values = na.omit(~annual_tmmx), group = "circles", title = paste("Annual TMMX Ave.", sep = ""), na.label = "", position = "topright", labFormat = labelFormat(suffix = paste(intToUtf8(176), "C"))) %>%
  
  
  addLabelOnlyMarkers(data = iPNW4_tmmx, lng = lat_long[,1], lat = lat_long[,2], label = lapply(labs, HTML), labelOptions = labelOptions(style = list("font-family" = "serif"), noHide = TRUE, direction = 'middle', textOnly = TRUE, textsize = "18px", col = "white",  offset = c(20, 0), markerOptions(riseOnHover = TRUE)
  )) 

addScaleBar(map, position = c("topright"), options = scaleBarOptions())




#--sum by county to get average 

iPNW3_pdsi <- aggregate(iPNW2$pdsi, by=list(iPNW2$state, iPNW2$county), FUN = "mean")
colnames(iPNW3_pdsi)<- c("STATE_NAME", "NAME", "annual_pdsi")

iPNW3_pdsi$STATE_NAME <- state.name[match(iPNW3_pdsi$STATE_NAME,state.abb)]

iPNW4_pdsi <- merge(counties, iPNW3_pdsi, by=c("STATE_NAME", "NAME"))

#plot map of Precipitation gradient

label <- paste(sep = "<br/>", iPNW4_pdsi$annual_pdsi)
markers <- data.frame(label)
labs <- lapply(seq(nrow(iPNW4_pdsi)), function(i) {
  paste0(as.character(round(iPNW4_pdsi@data[i, "annual_pdsi"],1)))})

exte <- as.vector(extent(iPNW4_pdsi))


pal2 <- colorNumeric(colorRampPalette(c("yellow", "red"))(11), na.color = "#ffffff",
                    domain = eval(parse(text=paste("iPNW4_pdsi$annual_pdsi", sep=""))))

pal <- colorNumeric(colorRampPalette(c("red", "yellow"))(11), na.color = "#ffffff",
                    domain = eval(parse(text=paste("iPNW4_pdsi$annual_pdsi", sep=""))))

map <- leaflet(data = iPNW4_pdsi, options = leafletOptions(zoomControl = FALSE, zoomSnap = 0.1)) %>%  
  addProviderTiles(providers$Hydda.Base) %>%
  addProviderTiles(providers$Stamen.TonerLines) %>%
  
  addMiniMap(
    tiles = providers$Stamen.TonerLite,
    position = 'topleft', 
    width = 100, height = 100,
    toggleDisplay = FALSE) %>%
  
  
  #addControl(title, position = "topleft", className="map-title") %>%
  
  
  fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = ~pal(iPNW4_pdsi$annual_pdsi),  fillOpacity = .8, weight = 1, stroke = TRUE, smoothFactor = 0.5, color = "black") %>% addPolygons(data = states, fillOpacity = 0, weight = 4, stroke = TRUE, smoothFactor = 0.5, color = "black") %>% 
  
  addLegend_decreasing(pal = pal2, values = na.omit(~annual_pdsi), group = "circles", title = paste("Annual PDSI Ave.", sep = ""), na.label = "", position = "topright", labFormat = labelFormat(suffix = "")) %>%
  
  
  addLabelOnlyMarkers(data = iPNW4_pdsi, lng = lat_long[,1], lat = lat_long[,2], label = lapply(labs, HTML), labelOptions = labelOptions(style = list("font-family" = "serif"), noHide = TRUE, direction = 'middle', textOnly = TRUE, textsize = "18px", col = "white",  offset = c(20, 0), markerOptions(riseOnHover = TRUE)
  )) 

addScaleBar(map, position = c("topright"), options = scaleBarOptions())








