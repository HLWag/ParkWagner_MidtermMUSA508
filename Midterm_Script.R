
##### SETUP #####
install.packages("geosphere")
library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(jtools)
library(mapview)
library(ggstance)
library(broom.mixed)
library(osmdata)
library(geosphere)

# functions
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

##### DATA WRANGLING & BUILDING FEATURES #####

#Read in Property Data (note that these are centroids, may need to convert to points for some analyses)
MiamiProperties <-
  st_read("C:/Users/wagne/Documents/GitHub/ParkWagner_MidtermMUSA508/studentsData.geojson")  
  ## for DP: "/Users/davidseungleepark/Library/Mobile Documents/com~apple~CloudDocs/Fall 2020/cpln592/ParkWagner_MidtermMUSA508/studentsData.geojson"
  #st_transform('ESRI:102658')
mapview::mapview(MiamiProperties)
st_crs(MiamiProperties)

#Build Features. Initial ideas: 

##AVAILABLE IN UNDERLYING DATA
  #neighborhood - I think Ken mentioned that he hasn't been able to find a neighborhood file for Miami, could we use zipcode as proxy (Mailing.Zip field)?
  #Lot size (LotSize field)  
  #living area (LivingSqFt field, Bed field, Bath field, Stories field? Select one I think)
  #age (based on the YearBuilt field)
  #house style (not sure this is available)
  #Extra feature codes (fields XF1, XF2, XF3; see metadata spreadsheet. Should we do something with these?)
  #Zoning (Zoning field)
  #the metadata spreadsheet also indicated there are fields for subareas and building element types, but I'm having a hard time locating those within the data

##TO FIND USING OPEN DATA - need to brainstorm this more, some initial ideas below
  #distance to coastline (I computed this and added the CoastDist field)
  #exposure to crime (assaults? these data are hard to find. jail bookings? https://gis-mdc.opendata.arcgis.com/datasets/jail-bookings-may-29-2015-to-current  )
  #distance to airport? available in OSM
  #Education (e.g., distance to nearest school?) available in OSM; school zone? - look into this?
  #Number of restaurants/bars nearby? Available in OSM
  #number of libraries nearby? Available in OSM
  #nearby parks/green space? 
  #number of retail/general commercial areas nearby (could be more specific like grocery stores?)

#OSM Data
miami.base <- 
  st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union()

xmin = st_bbox(miami.base)[[1]]
ymin = st_bbox(miami.base)[[2]]
xmax = st_bbox(miami.base)[[3]]  
ymax = st_bbox(miami.base)[[4]]

ggplot() +
  geom_sf(data=miami.base, fill="black") +
  geom_sf(data=st_as_sfc(st_bbox(miami.base)), colour="red", fill=NA) 

#Distance to Coastline 
Coastline<-opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature("natural", "coastline") %>%
  osmdata_sf()

#test in meters
dist<-geosphere::dist2Line(p=st_coordinates(st_centroid(MiamiProperties)),
                                            line=st_coordinates(Coastline$osm_lines)[,1:2])
#add to MiamiProperties and convert to miles
MiamiProperties <-
  MiamiProperties %>%  
  mutate(CoastDist=geosphere::dist2Line(p=st_coordinates(st_centroid(MiamiProperties)),
                                        line=st_coordinates(Coastline$osm_lines)[,1:2])*0.00062137)

hist(MiamiProperties$CoastDist) #values less than zero? or just weirdness with the plot

#Bars, pubs, and restaurants
bars<-opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("bar", "pub", "restaurant")) %>%
  osmdata_sf()

bars<-
  bars$osm_points%>%
  .[miami.base,]

ggplot()+
  geom_sf(data=miami.base, fill="black")+
  geom_sf(data=bars, colour="red", size=1)
  
MiamiProperties<-
  MiamiProperties %>% 
  mutate(
    bars_nn1= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),1),
    bars_nn2= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),2),
    bars_nn3= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),3),
    basr_nn4= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),4),
    bars_nn5= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),5))

#elementary school point data (2015 data)
Miami.elementary.point <-
st_read("https://opendata.arcgis.com/datasets/d3db0fce650d4e40a5949b0acae6fe3a_0.geojson") %>%
filter(CITY == "Miami" | CITY == "Miami Beach")

mapview::mapview(Miami.elementary.point)
st_crs(Miami.elementary.point)

#elemantary school boudaries (2018 data)

Miami.elementary.boudnary <-
  st_read("https://opendata.arcgis.com/datasets/19f5d8dcd9714e6fbd9043ac7a50c6f6_0.geojson") %>%
  filter(CITY == "Miami" | CITY == "Miami Beach")

mapview::mapview(Miami.elementary.point)
st_crs(Miami.elementary.point)
## will need to ggplot this data


#park facilities
Miami.parks <-  
  st_read("https://opendata.arcgis.com/datasets/8c9528d3e1824db3b14ed53188a46291_0.geojson") %>%
  filter(CITY == "Miami" | CITY == "Miami Beach")
  
mapview::mapview(Miami.parks)
st_crs(Miami.parks)  

#Crime- Sexual Offenders and Predators within Miami-Dade County point data 
Miami.sexualoffenders <-  
  st_read("https://opendata.arcgis.com/datasets/f8759d722aeb4198bfe7c4ad780604d2_0.geojson") %>%
  filter(CITY == "Miami" | CITY == "Miami Beach")

mapview::mapview(Miami.sexualoffenders)
st_crs(Miami.sexualoffenders) 



MiamiProperties$sexualoffenders_buffer =
  st_buffer(MiamiProperties, 660) %>% 
  aggregate(mutate(Miami.sexualoffenders, counter = 1),., sum) %>%
  pull(counter)

ggplot() + geom_sf(data = miami.base, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(Miami.sexualoffenders)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Aggravated Assaults, Boston") +
  mapTheme()

## Nearest Neighbor Feature
st_c <- st_coordinates

MiamiProperties <-
  MiamiProperties %>% 
  mutate(
    crime_nn1= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Miami.sexualoffenders),1),
    crime_nn2= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Miami.sexualoffenders),2),
    crime_nn3= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Miami.sexualoffenders),3),
    crime_nn4= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Miami.sexualoffenders),4),
    crime_nn5= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Miami.sexualoffenders),5))

## Plot NN count over space - Should increase or decrease?
MiamiProperties.plot <- MiamiProperties %>% 
  st_drop_geometry() %>% 
  dplyr::select(Folio, starts_with("crime_")) %>% 
  tidyr::pivot_longer(cols = -Folio, names_to = "crime_nn")

ggplot(MiamiProperties.plot, aes(x = crime_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "royalblue1") +
  theme_bw()



###### BUILD REGRESSION MODELS ######

#do we split the data into the training test and the test set before or after we build our regression models?
