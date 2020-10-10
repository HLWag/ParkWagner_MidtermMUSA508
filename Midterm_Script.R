
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

# Loading Miami base map from OSM Data
miami.base <- 
  st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union()

xmin = st_bbox(miami.base)[[1]]
ymin = st_bbox(miami.base)[[2]]
xmax = st_bbox(miami.base)[[3]]  
ymax = st_bbox(miami.base)[[4]]

ggplot() +
  geom_sf(data=miami.base, fill="gray30") +
  geom_sf(data=st_as_sfc(st_bbox(miami.base)), colour="red", fill=NA) 

# Loading elementary school boundaries 
elementary.school.boundaries <- 
  st_read("https://opendata.arcgis.com/datasets/19f5d8dcd9714e6fbd9043ac7a50c6f6_0.geojson") %>%
  st_transform('ESRI:102658')

# Read in Property Data (note that these are centroids, may need to convert to points for some analyses)
MiamiProperties_original <-
  st_read("C:/Users/wagne/Documents/GitHub/ParkWagner_MidtermMUSA508/studentsData.geojson")  
  ## for DP: st_read("/Users/davidseungleepark/Library/Mobile Documents/com~apple~CloudDocs/Fall 2020/cpln592/ParkWagner_MidtermMUSA508/studentsData.geojson")
  #st_transform('ESRI:102658')

st_crs(MiamiProperties) #note that I'm keeping this in the default WGS84 until I finish the CoastDist calculations
mapview::mapview(MiamiProperties)

MiamiProperties <-
  st_read("C:/Users/wagne/Documents/GitHub/ParkWagner_MidtermMUSA508/studentsData.geojson")%>%
  #st_read("/Users/davidseungleepark/Library/Mobile Documents/com~apple~CloudDocs/Fall 2020/cpln592/ParkWagner_MidtermMUSA508/studentsData.geojson") %>%
  
  mutate(pool = ifelse(str_detect(XF1, "Pool"), "Pool", "No Pool")) %>% 
  mutate(singlefamily = ifelse(str_detect(Zoning, "SINGLE FAMILY"), "Yes", "No")) %>%
  dplyr::select(-saleDate, -saleType, -saleQual, -County.Senior, -County.LongTermSenior, -County.Other.Exempt, -Owner1, -Owner2, 
                -Mailing.Address, -Mailing.City, -Mailing.State, -Mailing.Zip, -Mailing.Country, 
                -City.Senior, -City.LongTermSenior, -City.Other.Exempt, -Legal1, -Legal2, -Legal3, -Legal4, -Legal5, -Legal6, -XF1, -XF2, -XF3,
                -WVDB, -HEX, -GPAR, -County.2nd.HEX, -City.2nd.HEX, -MillCode, -Zoning, -Land.Use)

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

## Calculate the distance to Coastline (this calculation has to be in WGS84)
Coastline<-opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature("natural", "coastline") %>%
  osmdata_sf()

###test in meters
dist<-geosphere::dist2Line(p=st_coordinates(st_centroid(MiamiProperties)),
                                            line=st_coordinates(Coastline$osm_lines)[,1:2])

### add to MiamiProperties and convert to miles
MiamiProperties <-
  MiamiProperties %>%
  mutate(CoastDist=(geosphere::dist2Line(p=st_coordinates(st_centroid(MiamiProperties)),
                                        line=st_coordinates(Coastline$osm_lines)[,1:2])*0.00062137)[,1])

hist(MiamiProperties$CoastDist)

##Convert Miami Data to Local Projection #st_transform('ESRI:102658')
MiamiProperties <-
  MiamiProperties%>%
  st_transform('ESRI:102658')

## Add Data on Bars, pubs, and restaurants
bars <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("bar", "pub", "restaurant")) %>%
  osmdata_sf()

bars<-
  bars$osm_points%>%
  .[miami.base,]

bars<-
  bars%>%
  st_transform('ESRI:102658')
  
MiamiProperties<-
  MiamiProperties %>%
  mutate(
    bars_nn1= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),1),
    bars_nn2= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),2),
    bars_nn3= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),3),
    bars_nn4= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),4),
    bars_nn5= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(bars),5))

### plot yo visulize location
ggplot()+
  geom_sf(data=miami.base, fill="black")+
  geom_sf(data=bars, colour="red", size=1)

## Add data on crime- Sexual Offenders and Predators within Miami-Dade County point data 
miami.sexualoffenders <-  
  st_read("https://opendata.arcgis.com/datasets/f8759d722aeb4198bfe7c4ad780604d2_0.geojson") %>%
  filter(CITY == "MIAMI" | CITY == "MIAMI BEACH" | CITY == "Miami" | CITY == "Miami Beach") %>%
  st_transform('ESRI:102658')

mapview::mapview(miami.sexualoffenders)
st_crs(miami.sexualoffenders) 

MiamiProperties$sexualoffenders_buffer =
  st_buffer(MiamiProperties, 660) %>% 
  aggregate(mutate(miami.sexualoffenders, counter = 1),., sum) %>%
  pull(counter)

ggplot() + geom_sf(data = miami.base, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(miami.sexualoffenders)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Sexual Offenders in Miami") +
  mapTheme()

MiamiProperties <-
  MiamiProperties %>% 
  mutate(
    crime_nn1= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(miami.sexualoffenders),1),
    crime_nn2= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(miami.sexualoffenders),2),
    crime_nn3= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(miami.sexualoffenders),3),
    crime_nn4= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(miami.sexualoffenders),4),
    crime_nn5= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(miami.sexualoffenders),5))

### plot to understand the data
MiamiProperties.plot <- MiamiProperties %>% 
  st_drop_geometry() %>% 
  dplyr::select(Folio, starts_with("crime_")) %>% 
  tidyr::pivot_longer(cols = -Folio, names_to = "crime_nn")

ggplot(MiamiProperties.plot, aes(x = crime_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "royalblue1") +
  theme_bw()

## Add the data on Park Facilities
Parks<-st_read("https://opendata.arcgis.com/datasets/8c9528d3e1824db3b14ed53188a46291_0.geojson")%>%
st_transform('ESRI:102658')
mapview::mapview(Parks)

MiamiProperties<-
  MiamiProperties %>% 
  mutate(
    parks_nn1= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Parks),1),
    parks_nn2= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Parks),2),
    parks_nn3= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Parks),3),
    parks_nn4= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Parks),4),
    parks_nn5= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(Parks),5))

#TOD or non-TOD; distance to transit stop?
metrorail_stop<-st_read("https://opendata.arcgis.com/datasets/ee3e2c45427e4c85b751d8ad57dd7b16_0.geojson")%>%
  st_transform('ESRI:102658')%>%
  dplyr::select(NAME)

metromover_stop<-st_read("https://opendata.arcgis.com/datasets/aec76104165c4e879b9b0203fa436dab_0.geojson")%>%
  st_transform('ESRI:102658')%>%
  dplyr::select(NAME)

metro_stops<-
  rbind(metromover_stop,metrorail_stop)

#Distance to metro_stop (Added a column for the distance to the nearest stop and a column for homes that are within 0.5 miles of a stop)
MiamiProperties<-
  MiamiProperties %>% 
  mutate(
    metro_nn1= nn_function(st_coordinates(st_centroid(MiamiProperties)),st_coordinates(metro_stops),1),
    TOD=ifelse(metro_nn1<2640,"TOD","Non-TOD"))

###### BUILD REGRESSION MODELS ######

#do we split the data into the training test and the test set before or after we build our regression models?

reg1 <- lm(SalePrice ~ ., data = st_drop_geometry(MiamiProperties) %>% 
            dplyr::select(SalePrice, Bed, Bath, Stories, YearBuilt, LivingSqFt, Mailing.Zip, bars_nn5, CoastDist, parks_nn5))
summ(reg1)
summary(reg1)





##### CORRELATION #####
#Seleting between multiple nn variables:
st_drop_geometry(MiamiProperties) %>% 
  dplyr::select(SalePrice, crime_nn1, crime_nn2, crime_nn3, crime_nn4, crime_nn5) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Correlations between Sale Price and Crime Features") +
  plotTheme()

st_drop_geometry(MiamiProperties) %>% 
  dplyr::select(SalePrice, bars_nn1, bars_nn2, bars_nn3, bars_nn4, bars_nn5) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Correlations between Sale Price and Bar/Restaurant Features") +
  plotTheme()

st_drop_geometry(MiamiProperties) %>% 
  dplyr::select(SalePrice, parks_nn1, parks_nn2, parks_nn3, parks_nn4, parks_nn5) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Correlations between Sale Price and Park Features") +
  plotTheme()

#Price as a function of continuous variables (we need four of these for the assignment)
st_drop_geometry(MiamiProperties) %>% 
  mutate(Age = 2020 - YearBuilt) %>%
  dplyr::select(SalePrice, LivingSqFt, Age, CoastDist, bars_nn1, bars_nn2, crime_nn1, crime_nn2, parks_nn1, parks_nn2, metro_nn1) %>%
  filter(SalePrice <= 1000000, Age < 500) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

#Price as a function of categorical variables
st_drop_geometry(MiamiProperties) %>%
  dplyr::select(SalePrice, TOD, Stories, pool, singlefamily)%>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable,Value, -SalePrice)%>%
  ggplot(aes(Value, SalePrice))+
  geom_bar(position="dodge",stat="summary", fun.y="mean")+
  facet_wrap(~Variable, ncol=1, scales="free")+
  plotTheme()

#Correlation Matrix
numericVars <- 
  select_if(st_drop_geometry(MiamiProperties), is.numeric) %>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

#Map of dependent variable (sale price)


#3 maps of independent variables

ggplot()+
  geom_sf(data=elementary.school.boundaries)+
  labs(title="Elementary School Districts") +
  mapTheme()
