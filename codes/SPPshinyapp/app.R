#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('rgdal', 'sf', 'tmap', 'tidyverse', 'sp', 'rgeos','maptools', 'raster', 'spatstat', 'tmaptools', 'spdep', 'OpenStreetMap','shiny', 'SpatialPosition')
for (p in packages){
    if (!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

#importing popdata dataset
popdata <- read_csv('data/planning-area-subzone-age-group-sex-and-type-of-dwelling-june-2011-2019.csv')

#grouping residents by agegroup
popdata2019 <- popdata %>%
    filter(year == 2019) %>%
    filter(age_group == "65_to_69" | age_group == "70_to_74" | age_group == '75_to_79' | age_group == '80_to_84' | age_group == '85_to_89' | age_group == '90_and_over') %>%
    group_by(planning_area, subzone, age_group) %>%
    summarise(elderly_count = sum(resident_count)) %>%
    ungroup() %>%
    spread(age_group, elderly_count) %>%
    mutate(elderly_count = `65_to_69` + `70_to_74` + `75_to_79` + `80_to_84` + `85_to_89` + `90_and_over`)

popdata2019 <- mutate_at(popdata2019, .vars = c("subzone", "planning_area"), .funs=toupper)

#Importing mp14 subzone data
mpsz_sf <- st_read(dsn='data', layer='MP14_SUBZONE_WEB_PL')
mpsz_sf <- mpsz_sf %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N, REGION_N, X_ADDR, Y_ADDR, SHAPE_Leng, SHAPE_Area, geometry)
mpsz_sf <- st_transform(mpsz_sf, 3414)
st_crs(mpsz_sf)


#Importing Eldercare Data
eldercare_sf <- st_read(dsn='data', layer='ELDERCARE')

eldercare_sf <- eldercare_sf %>%
  dplyr::select(NAME, ADDRESSPOS, ADDRESSSTR, X_ADDR, Y_ADDR, geometry) %>%
  mutate(label = "Eldercare centres") %>%
  mutate(capacity = 1)
eldercare_sf <- st_transform(eldercare_sf, 3414)
st_crs(eldercare_sf)

#Importing Silverincomm Data
infocomm_sf <- st_read(dsn='data', layer='SILVERINFOCOMM')
infocomm_sf <- infocomm_sf %>%
  dplyr::select(NAME, ADDRESSBUI, ADDRESSPOS, X_ADDR, Y_ADDR, geometry) %>%
  mutate(label = "Silver Infocomm Junc") %>%
  mutate(capacity = 1)
infocomm_sf <- st_transform(infocomm_sf, 3414)
st_crs(infocomm_sf)

#Importing chas clinic data
chas_sf <- st_read(dsn='data/chas-clinics-kml.kml')
chas_sf <- chas_sf %>%
    mutate(label = "Chas Clinics") %>%
    mutate(capacity = 1)
chas_sf <- st_transform(chas_sf, 3414)
st_crs(chas_sf)

#Importing sg costal outline
sg <- readOGR(dsn = "data", layer="CostalOutline")
sg_spatialpoint <- as(sg, "SpatialPolygons")




#Preparing mpsz demand
mpsz_demand <- left_join(mpsz_sf, popdata2019, by=c('PLN_AREA_N' = 'planning_area', 'SUBZONE_N' = 'subzone'))
summary(mpsz_demand)

mpsz_demand_sp <- as_Spatial(mpsz_demand)
mpsz_demand_spatialpoint <- as(mpsz_demand_sp, "SpatialPolygons")



#Further wrangle the data 
eldercare_sf <- st_join(eldercare_sf, mpsz_sf, join=st_intersects)
eldercare_sp <- as_Spatial(eldercare_sf)
eldercare_spatialpoint <- as(eldercare_sp, "SpatialPoints")

infocomm_sf <- st_join(infocomm_sf, mpsz_sf, join=st_intersects)
infocomm_sp <- as_Spatial(infocomm_sf)
infocomm_spatialpoint <- as(infocomm_sp, "SpatialPoints")

chas_sf <- st_join(chas_sf, mpsz_sf, join=st_intersects)
chas_sp <- as_Spatial(chas_sf)
chas_spatialpoint <- as(chas_sp, "SpatialPoints")

#Creating centroids
centroids <- st_centroid(mpsz_sf)
centroids <- st_join(centroids, mpsz_demand, join=st_intersects)
#Convering sf_mpsz to sp_mpsz
mpsz_sp <- as_Spatial(mpsz_sf)
mpsz_spatialpoint <- as(mpsz_sp, "SpatialPolygons")

#reading OSM 
sg_osm <- read_osm(mpsz_spatialpoint, ext=1.3)

#Extracting planning areas(Tampines,Amk,Bishan,Jurong West)
tp <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == "TAMPINES",]
amk <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == "ANG MO KIO",]
bs <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == "BISHAN",]
jw <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == "JURONG WEST",]

tp_sf <- mpsz_sf[mpsz_sf$PLN_AREA_N == "TAMPINES",]
amk_sf <- mpsz_sf[mpsz_sf$PLN_AREA_N == "ANG MO KIO",]
bs_sf <- mpsz_sf[mpsz_sf$PLN_AREA_N == "BISHAN",]
jw_sf <- mpsz_sf[mpsz_sf$PLN_AREA_N == "JURONG WEST",]

#Extracting centroids by selected planning areas
tp_centroids <- centroids[centroids$PLN_AREA_N == 'TAMPINES'] 
amk_centroids <- centroids[centroids$PLN_AREA_N == 'ANG MO KIO'] 
bs_centroids <- centroids[centroids$PLN_AREA_N == 'BISHAN'] 
jw_centroids <- centroids[centroids$PLN_AREA_N == 'JURONG WEST'] 

#Extracting Eldercare, Infocomm, CHAS within study area
tp_eldercare_sf <- eldercare_sf %>% filter(PLN_AREA_N == 'TAMPINES')
amk_eldercare_sf <- eldercare_sf %>% filter(PLN_AREA_N == 'ANG MO KIO')
bs_eldercare_sf <- eldercare_sf %>% filter(PLN_AREA_N == 'BISHAN')
jw_eldercare_sf <- eldercare_sf %>% filter(PLN_AREA_N == 'JURONG WEST')

tp_infocomm_sf <- infocomm_sf %>% filter(PLN_AREA_N == 'TAMPINES')
amk_infocomm_sf <- infocomm_sf %>% filter(PLN_AREA_N == 'ANG MO KIO')
bs_infocomm_sf <- infocomm_sf %>% filter(PLN_AREA_N == 'BISHAN')
jw_infocomm_sf <- infocomm_sf %>% filter(PLN_AREA_N == 'JURONG WEST')

tp_chas_sf <- chas_sf %>% filter(PLN_AREA_N == 'TAMPINES')
amk_chas_sf <- chas_sf %>% filter(PLN_AREA_N == 'ANG MO KIO')
bs_chas_sf <- chas_sf %>% filter(PLN_AREA_N == 'BISHAN')
jw_chas_sf <- chas_sf %>% filter(PLN_AREA_N == 'JURONG WEST')



#Convert planning areas into generic spatial polygons objects
tp_spatialpoint <- as(tp, "SpatialPolygons")
amk_spatialpoint <- as(amk, "SpatialPolygons")
bs_spatialpoint <- as(bs, "SpatialPolygons")
jw_spatialpoint <- as(jw, "SpatialPolygons")

#Creating owin objects based on planning areas
tp_owin <- as(tp, "owin")
amk_owin <- as(amk, "owin")
bs_owin <- as(bs, "owin")
jw_owin <- as(jw, "owin")
sg_owin <- as(mpsz_sp, "owin")

#converting facilities spatialpoints to spatstat ppp
chas_ppp <- as(chas_spatialpoint, "ppp")
infocomm_ppp <- as(infocomm_spatialpoint, "ppp")
eldercare_ppp <- as(eldercare_spatialpoint, "ppp")

#using jittering to better visualize overlapped facility points
chas_ppp_jit <- rjitter(chas_ppp, retry=TRUE, nsim=1, drop=TRUE)
infocomm_ppp_jit <- rjitter(infocomm_ppp, retry=TRUE, nsim=1, drop=TRUE)
eldercare_ppp_jit <- rjitter(eldercare_ppp, retry=TRUE, nsim=1, drop=TRUE)

#Combining Chas, Infocomm and Eldercare with study area own
ppp_chas_tp <- chas_ppp_jit[tp_owin]
ppp_chas_amk <- chas_ppp_jit[amk_owin]
ppp_chas_bs <- chas_ppp_jit[bs_owin]
ppp_chas_jw <- chas_ppp_jit[jw_owin]

ppp_infocomm_tp <- infocomm_ppp_jit[tp_owin]
ppp_infocomm_amk <- infocomm_ppp_jit[amk_owin]
ppp_infocomm_bs <- infocomm_ppp_jit[bs_owin]
ppp_infocomm_jw <- infocomm_ppp_jit[jw_owin]

ppp_eldercare_tp <- eldercare_ppp_jit[tp_owin]
ppp_eldercare_amk <- eldercare_ppp_jit[amk_owin]
ppp_eldercare_bs <- eldercare_ppp_jit[bs_owin]
ppp_eldercare_jw <- eldercare_ppp_jit[jw_owin]

#Convert data to KM by rescaling
ppp_chas_tp.km = rescale(ppp_chas_tp, 1000, "km")
ppp_chas_amk.km = rescale(ppp_chas_amk, 1000, "km")
ppp_chas_bs.km = rescale(ppp_chas_bs, 1000, "km")
ppp_chas_jw.km = rescale(ppp_chas_jw, 1000, "km")

ppp_infocomm_tp.km = rescale(ppp_infocomm_tp, 1000, "km")
ppp_infocomm_amk.km = rescale(ppp_infocomm_amk, 1000, "km")
ppp_infocomm_bs.km = rescale(ppp_infocomm_bs, 1000, "km")
ppp_infocomm_jw.km = rescale(ppp_infocomm_jw, 1000, "km")

ppp_eldercare_tp.km = rescale(ppp_eldercare_tp, 1000, "km")
ppp_eldercare_amk.km = rescale(ppp_eldercare_amk, 1000, "km")
ppp_eldercare_bs.km = rescale(ppp_eldercare_bs, 1000, "km")
ppp_eldercare_jw.km = rescale(ppp_eldercare_jw, 1000, "km")

#Extracting all facilities within the boundaries of the study area
ppp_chas_mpsz = chas_ppp_jit[sg_owin]
ppp_infocomm_mpsz = infocomm_ppp_jit[sg_owin]
ppp_eldercare_mpsz = eldercare_ppp_jit[sg_owin]

#converting to KM
ppp_chas_mpsz.km = rescale(ppp_chas_mpsz, 1000, "km")
ppp_infocomm_mpsz.km = rescale(ppp_infocomm_mpsz, 1000, "km")
ppp_eldercare_mpsz.km = rescale(ppp_eldercare_mpsz, 1000, "km")

#performing CSR test
K_chas_tp = Lest(ppp_chas_tp, correction = "Ripley")
K_chas_amk = Lest(ppp_chas_amk, correction = "Ripley")
K_chas_bs = Lest(ppp_chas_bs, correction = "Ripley")
K_chas_jw = Lest(ppp_chas_jw, correction = "Ripley")

K_infocomm_tp = Lest(ppp_infocomm_tp, correction = "Ripley")
K_infocomm_amk = Lest(ppp_infocomm_amk, correction = "Ripley")
K_infocomm_bs = Lest(ppp_infocomm_bs, correction = "Ripley")
K_infocomm_jw = Lest(ppp_infocomm_jw, correction = "Ripley")

K_eldercare_tp = Lest(ppp_eldercare_tp, correction = "Ripley")
K_eldercare_amk = Lest(ppp_eldercare_tp, correction = "Ripley")
K_eldercare_bs = Lest(ppp_eldercare_tp, correction = "Ripley")
K_eldercare_jw = Lest(ppp_eldercare_tp, correction = "Ripley")

#Creating envelops
#K_chas_tp.csr <- envelope(ppp_chas_tp, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_chas_amk.csr <- envelope(ppp_chas_amk, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_chas_bs.csr <- envelope(ppp_chas_bs, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_chas_jw.csr <- envelope(ppp_chas_jw, Kest, nsim = 99, rank = 1, glocal=TRUE)

#K_infocomm_tp.csr <- envelope(ppp_infocomm_tp, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_infocomm_amk.csr <- envelope(ppp_infocomm_amk, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_infocomm_bs.csr <- envelope(ppp_infocomm_bs, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_infocomm_jw.csr <- envelope(ppp_infocomm_jw, Kest, nsim = 99, rank = 1, glocal=TRUE)

#K_eldercare_tp.csr <- envelope(ppp_eldercare_tp, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_eldercare_amk.csr <- envelope(ppp_eldercare_tp, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_eldercare_bs.csr <- envelope(ppp_eldercare_tp, Kest, nsim = 99, rank = 1, glocal=TRUE)
#K_eldercare_jw.csr <- envelope(ppp_eldercare_tp, Kest, nsim = 99, rank = 1, glocal=TRUE)

#KDE codes start from here
#Create a binding box for selected subzones
tp_bb <- st_bbox(mpsz_sf %>% filter(PLN_AREA_N == "TAMPINES"))
amk_bb <- st_bbox(mpsz_sf %>% filter(PLN_AREA_N == "ANG MO KIO"))
bs_bb <- st_bbox(mpsz_sf %>% filter(PLN_AREA_N == "BISHAN"))
jw_bb <- st_bbox(mpsz_sf %>% filter(PLN_AREA_N == "JURONG WEST"))

#Getting osm for the indivisual planning areas
tp_osm <- read_osm(tp_bb, ext=1.1)
amk_osm <- read_osm(amk_bb, ext=1.1)
bs_osm <- read_osm(bs_bb, ext=1.1)
jw_osm <- read_osm(jw_bb, ext=1.1)

#Function to plot KDE
getPlnAreaKernelDensityMap <- function(osm, pln, ppp, ppp_str) {
    ppp.km <- rescale(ppp, 1000, "km")
    
    kde <- density(ppp.km, sigma=0.20, edge=TRUE, kernel="gaussian")
    
    gridded_kde_bw <- as.SpatialGridDataFrame.im(kde)
    
    kde_bw_raster <- raster(gridded_kde_bw)
    
    projection(kde_bw_raster) <- crs("+init=EPSG:3414 +datum=WGS84 +units=km")
    
    # Plot kernel density map on openstreetmap
    tm_shape(osm)+ 
        tm_layout(legend.outside = TRUE, title=ppp_str)+
        tm_rgb()+
        tm_shape(pln)+
        tm_borders(col = "darkblue", lwd = 2, lty="longdash")+
        tm_shape(kde_bw_raster) + 
        tm_raster("v", alpha=0.5,  
                  palette = "YlOrRd")
}

#plot all KDE maps
kde_chas_tp <- getPlnAreaKernelDensityMap(tp_osm, tp, ppp_chas_tp, "Chas Clinics in Tampines") 
kde_infocomm_tp <- getPlnAreaKernelDensityMap(tp_osm, tp, ppp_infocomm_tp, "Infocomm Centres in Tampines")
kde_eldercare_tp <- getPlnAreaKernelDensityMap(tp_osm, tp, ppp_eldercare_tp, "Eldercare Services in Tampines")

kde_chas_amk <- getPlnAreaKernelDensityMap(amk_osm, amk, ppp_chas_amk, "Chas Clinics in Ang Mo Kio") 
kde_infocomm_amk <- getPlnAreaKernelDensityMap(amk_osm, amk, ppp_infocomm_amk, "Infocomm Centres in Ang Mo Kio")
kde_eldercare_amk <- getPlnAreaKernelDensityMap(amk_osm, amk, ppp_eldercare_amk, "Eldercare Services in Ang Mo Kio")

kde_chas_bs <- getPlnAreaKernelDensityMap(bs_osm, bs, ppp_chas_bs, "Chas Clinics in Bishan") 
kde_infocomm_bs <- getPlnAreaKernelDensityMap(bs_osm, bs, ppp_infocomm_bs, "Infocomm Centres in Bishan")
kde_eldercare_bs <- getPlnAreaKernelDensityMap(bs_osm, bs, ppp_eldercare_bs, "Eldercare Services in Bishan")

kde_chas_jw <- getPlnAreaKernelDensityMap(jw_osm, jw, ppp_chas_jw, "Chas Clinics in Jurong West") 
kde_infocomm_jw <- getPlnAreaKernelDensityMap(jw_osm, jw, ppp_infocomm_jw, "Infocomm Centres in Jurong West")
kde_eldercare_jw <- getPlnAreaKernelDensityMap(jw_osm, jw, ppp_eldercare_jw, "Eldercare Services in Jurong West")


#function for creating boxmap
boxbreaks <- function(v,mult=1.5) {
    qv <- unname(quantile(v))
    iqr <- qv[4] - qv[2]
    upfence <- qv[4] + mult * iqr
    lofence <- qv[2] - mult * iqr
    # initialize break points vector. might need more length for bigger data
    bb <- vector(mode="numeric",length=7)
    # logic for lower and upper fences
    if (lofence < qv[1]) { # no lower outliers
        bb[1] <- lofence
        bb[2] <- floor(qv[1])
    } else {
        bb[2] <- lofence
        bb[1] <- qv[1]
    }
    if (upfence > qv[5]) { # no upper outliers
        bb[7] <- upfence
        bb[6] <- ceiling(qv[5])
    } else {
        bb[6] <- upfence
        bb[7] <- qv[5]
    }
    bb[3:5] <- qv[2:4]
    return(bb)
}

get.var <- function(vname,df) {
    v <- df[vname] %>% st_set_geometry(NULL)
    v <- unname(v[,1])
    return(v)
}

boxmap <- function(vnam,df,legtitle=NA,mtitle="Box Map",mult=1.5){
    var <- get.var(vnam,df)
    bb <- boxbreaks(var)
    tm_shape(df) +
        tm_fill(vnam,title=legtitle,breaks=bb,palette="-RdBu",
                labels = c("lower outlier", "< 25%", "25% - 50%", "50% - 75%","> 75%", "upper outlier")) +
        tm_borders() +
        tm_layout(title = mtitle, title.position = c("right","bottom"))
}

#Accessibility Analysis 
#Creating Distance Matrix
tp_eldercare_dismat <- CreateDistMatrix(knownpts=tp_centroids, unknownpts=tp_eldercare_sf, longlat=FALSE)
amk_eldercare_dismat <- CreateDistMatrix(knownpts=amk_centroids, unknownpts=amk_eldercare_sf, longlat=FALSE)
bs_eldercare_dismat <- CreateDistMatrix(knownpts=bs_centroids, unknownpts=bs_eldercare_sf, longlat=FALSE)
jw_eldercare_dismat <- CreateDistMatrix(knownpts=jw_centroids, unknownpts=jw_eldercare_sf, longlat=FALSE)

tp_infocomm_dismat <- CreateDistMatrix(knownpts=tp_centroids, unknownpts=tp_infocomm_sf, longlat=FALSE)
amk_infocomm_dismat <- CreateDistMatrix(knownpts=amk_centroids, unknownpts=amk_infocomm_sf, longlat=FALSE)
bs_infocomm_dismat <- CreateDistMatrix(knownpts=bs_centroids, unknownpts=bs_infocomm_sf, longlat=FALSE)
jw_infocomm_dismat <- CreateDistMatrix(knownpts=jw_centroids, unknownpts=jw_infocomm_sf, longlat=FALSE)

tp_chas_dismat <- CreateDistMatrix(knownpts=tp_centroids, unknownpts=tp_chas_sf, longlat=FALSE)
amk_chas_dismat <- CreateDistMatrix(knownpts=amk_centroids, unknownpts=amk_chas_sf, longlat=FALSE)
bs_chas_dismat <- CreateDistMatrix(knownpts=bs_centroids, unknownpts=bs_chas_sf, longlat=FALSE)
jw_chas_dismat <- CreateDistMatrix(knownpts=jw_centroids, unknownpts=jw_chas_sf, longlat=FALSE)

#Convert the unit from m to km
tp_eldercare_dismat <- as.matrix(tp_eldercare_dismat/1000)
amk_eldercare_dismat <- as.matrix(amk_eldercare_dismat/1000)
bs_eldercare_dismat <- as.matrix(bs_eldercare_dismat/1000)
jw_eldercare_dismat <- as.matrix(jw_eldercare_dismat/1000)

tp_infocomm_dismat <- as.matrix(tp_infocomm_dismat/1000)
amk_infocomm_dismat <- as.matrix(amk_infocomm_dismat/1000)
bs_infocomm_dismat <- as.matrix(bs_infocomm_dismat/1000)
jw_infocomm_dismat <- as.matrix(jw_infocomm_dismat/1000)

tp_chas_dismat <- as.matrix(tp_chas_dismat/1000)
amk_chas_dismat <- as.matrix(amk_chas_dismat/1000)
bs_chas_dismat <- as.matrix(bs_chas_dismat/1000)
jw_chas_dismat <- as.matrix(jw_chas_dismat/1000)


#Function to compute accessibility values & Visualising Output
getPA_SAM <- function(demand, supply, dist_matrix, sf) {
  
  #Compute accessibility matrix
  temp <- data.frame(ac(demand$elderly_count,
                        supply$capacity, 
                        dist_matrix,
                        d0 = 30, 
                        power = 2, 
                        family = 'SAM'))
  colnames(temp) <- 'accSAM'
  temp <- tibble::as_tibble(temp)
  result <- bind_cols(sf, temp)
  return(result)
}






ui <- navbarPage("IS415 Team2",
           tabPanel("EDA",
                    fixedRow(
                        column(8,
                               titlePanel("Spatial Point Pattern Analysis"),
                               fixedRow(
                                   column(10,
                                          tmapOutput("boxplot")
                                   ),
                                   column(10,
                                          tmapOutput("sdplot")
                                   )
                               )
                        )
                    )),
           tabPanel("Kernal Density Plots",
                    fixedRow(
                        column(12,
                               titlePanel("Kernal Density Plots"),
                               fixedRow(
                                   column(2,
                                          selectInput('planningarea', 'Select Planning Area', choices = c("Tampines", "Ang Mo Kio", "Bishan", "Jurong West"))  
                                   ),
                                   column(4
                                          
                                   ),
                                   column(4
                                   )
                               )
                        )
                    )),
           tabPanel("Accessibility", 
                    fixedRow(
                      column(12, 
                             titlePanel('Geographical Accessibility by Planning Area'),
                             sidebarLayout(
                               sidebarPanel(
                                 
                                 'Select Planning Area', 
                                 radioButtons(inputId='PLN_AREA_N', 
                                        label='Planning Area', 
                                        choices=c('Tampines' = 'TAMPINES', 
                                                  'Ang Mo Kio' = 'ANG MO KIO', 
                                                  'Bishan' = 'BISHAN', 
                                                  'Jurong West' = 'JURONG WEST'), 
                                        selected='Tampines'), 
                                 
                                 'Select Facility', 
                                 radioButtons(inputId='facilityType', 
                                              label='Facility Type', 
                                              choices=c('Eldercare Centres' = 'Eldercare Centres', 
                                                        'Silver Infocomm Junctions' = 'Silver Infocomm Junctions', 
                                                        'CHAS Clinics' = 'CHAS Clinics'), 
                                              selected='Eldercare Centres'), 
                                 
                                 actionButton('SAMgoButton', 'Go!')
                               ), 
                               
                               mainPanel(
                                 tmapOutput('SAMmap')
                               )
                             )
                             )
                    ))
)


server <- function(input, output) {

    output$boxplot <- renderTmap({
        boxmap("elderly_count", mpsz_demand, mtitle="Elderly Distribution")
    })
    
    #Supply and Demand of 3 different elder care facilities
    output$sdplot <- renderTmap({
        tm_shape(mpsz_demand) + 
            tm_fill(c("elderly_count"),
                    title = "Elderly Population",
                    breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Population"="elderly_count"),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1) +
            tm_shape(chas_sf) + tm_dots("red") +
            tm_shape(eldercare_sf) + tm_dots("blue")+
            tm_shape(infocomm_sf) + tm_dots("green") 
    })
    
      
    generateSAM <- eventReactive(input$SAMgoButton, {
      
      x <- isolate(input$PLN_AREA_N)
      y <- isolate(input$facilityType)
      
      if(x=='TAMPINES'){
        demand <- tp_centroids
        sf <- tp_sf
        
        if (y=='Eldercare Centres'){
          supply <- tp_eldercare_sf
          dist_mat <- tp_eldercare_dismat
        }
        else if (y=='Silver Infocomm Junctions'){
          supply <- tp_infocomm_sf
          dist_mat <- tp_infocomm_dismat
        }
        else if(y=='CHAS Clinics'){
          supply <- tp_chas_sf
          dist_mat <- tp_chas_dismat
        }
      }
      
      else if(x=='ANG MO KIO'){
        demand <- amk_centroids
        sf <- amk_sf
        
        if (y=='Eldercare Centres'){
          supply <- amk_eldercare_sf
          dist_mat <- amk_eldercare_dismat
        }
        else if (y=='Silver Infocomm Junctions'){
          supply <- amk_infocomm_sf
          dist_mat <- amk_infocomm_dismat
        }
        else if(y=='CHAS Clinics'){
          supply <- amk_chas_sf
          dist_mat <- amk_chas_dismat
        }
      }
      
      else if(x=='BISHAN'){
        demand <- bs_centroids
        sf <- bs_sf
        
        if (y=='Eldercare Centres'){
          supply <- bs_eldercare_sf
          dist_mat <- bs_eldercare_dismat
        }
        else if (y=='Silver Infocomm Junctions'){
          supply <- bs_infocomm_sf
          dist_mat <- bs_infocomm_dismat
        }
        else if(y=='CHAS Clinics'){
          supply <- bs_chas_sf
          dist_mat <- bs_chas_dismat
        }
      }
      
      else if(x=='JURONG WEST'){
        demand <- jw_centroids
        sf <- jw_sf
        
        if (y=='Eldercare Centres'){
          supply <- jw_eldercare_sf
          dist_mat <- jw_eldercare_dismat
        }
        else if (y=='Silver Infocomm Junctions'){
          supply <- jw_infocomm_sf
          dist_mat <- jw_infocomm_dismat
        }
        else if(y=='CHAS Clinics'){
          supply <- jw_chas_sf
          dist_mat <- jw_chas_dismat
        }
      }
      
      result <- getPA_SAM(demand, supply, dist_mat, sf)
      tm_shape(result)+ 
        tm_borders(alpha = 0.5) + 
        tm_fill(col='accSAM', 
                style='pretty')
      
      })
      
    output$SAMmap <- renderTmap({
      map <- generateSAM()
      map
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
