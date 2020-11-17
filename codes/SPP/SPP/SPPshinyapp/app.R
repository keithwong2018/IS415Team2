#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('rgdal', 'sf', 'tmap', 'tidyverse', 'sp', 'rgeos','maptools', 'raster', 'spatstat', 'tmaptools', 'spdep', 'OpenStreetMap', 'ggpubr', 'SpatialPosition', 'SpatialAcc', 'dplyr', 'shinycssloaders')
for (p in packages){
    if (!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}


# Population Data
# columns = planning_area, subzone, elderly_count, total_count
popdata <- read_csv('data/planning-area-subzone-age-group-sex-and-type-of-dwelling-june-2011-2019.csv')
popdata2019 <- popdata %>%
  filter(year == 2019) %>%
  # filter(age_group == "65_to_69" | age_group == "70_to_74" | age_group == '75_to_79' | age_group == '80_to_84' | age_group == '85_to_89' | age_group == '90_and_over') %>%
  group_by(planning_area, subzone, age_group) %>%
  summarise(count = sum(resident_count)) %>%
  ungroup() %>%
  spread(age_group, count) %>%
  mutate(elderly_count = `65_to_69` + `70_to_74` + `75_to_79` + `80_to_84` + `85_to_89` + `90_and_over`) %>%
  mutate(total_count = rowSums(.[3:21])) %>%
  dplyr::select(planning_area, subzone, elderly_count, total_count)
popdata2019 <- mutate_at(popdata2019, .vars = c("subzone", "planning_area"), .funs=toupper)

# Eldercare Data
eldercare_sf <- st_read(dsn='data', layer='ELDERCARE')
eldercare_sf <- eldercare_sf %>%
    mutate(label = "Eldercare centres") 
eldercare_sf <- st_transform(eldercare_sf, 3414)
st_crs(eldercare_sf)
eldercare_sp <- as_Spatial(eldercare_sf)
eldercare_spatialpoint <- as(eldercare_sp, "SpatialPoints")

# Silver Infocomm Data
infocomm_sf <- st_read(dsn='data', layer='SILVERINFOCOMM')
infocomm_sf <- infocomm_sf %>%
    mutate(label = "Silver Infocomm Junc")
infocomm_sf <- st_transform(infocomm_sf, 3414)
st_crs(infocomm_sf)
infocomm_sp <- as_Spatial(infocomm_sf)
infocomm_spatialpoint <- as(infocomm_sp, "SpatialPoints")

# Chas Clinic Data
chas_sf <- st_read(dsn='data/chas-clinics-kml.kml')
chas_sf <- chas_sf %>%
    mutate(label = "Chas Clinics") %>%
    mutate(capacity = 1)
chas_sf <- st_transform(chas_sf, 3414)
st_crs(chas_sf)
chas_sp <- as_Spatial(chas_sf)
chas_spatialpoint <- as(chas_sp, "SpatialPoints")

# MP14 Subzone Data
mpsz_sf <- st_read(dsn='data', layer='MP14_SUBZONE_WEB_PL')
mpsz_sf <- mpsz_sf %>%
    dplyr::select(SUBZONE_N, PLN_AREA_N, REGION_N, X_ADDR, Y_ADDR, SHAPE_Leng, SHAPE_Area, geometry)
mpsz_sf <- st_transform(mpsz_sf, 3414)
st_crs(mpsz_sf)

eldercare_sf <- st_join(eldercare_sf, mpsz_sf, join=st_intersects)
infocomm_sf <- st_join(infocomm_sf, mpsz_sf, join=st_intersects)
chas_sf <- st_join(chas_sf, mpsz_sf, join=st_intersects)

# HDB Data
hdb <- read_csv('data/hdb_data.csv')
hdb <- hdb %>% 
  mutate(total_count=rowSums(.[2:11])) %>%
  dplyr::select(Postcode, X, Y, Latitude, Longitude, total_count)
hdb$Postcode <- as.character(hdb$Postcode)
hdb_sf <- st_as_sf(hdb, coords=c('X', 'Y'), crs='EPSG:3414')

# SG Coastal Outline
sg <- readOGR(dsn = "data", layer="CostalOutline")
sg_spatialpoint <- as(sg, "SpatialPolygons")

# Compute elderly_density in mpsz
mpsz_sf <- left_join(mpsz_sf, popdata2019, by=c('PLN_AREA_N' = 'planning_area', 'SUBZONE_N' = 'subzone'))
mpsz_sf <- mpsz_sf %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N, REGION_N, elderly_count, total_count, geometry) %>%
  mutate(elderly_proportion = elderly_count / total_count) %>%
  mutate_if(is.numeric, ~replace(., is.nan(.), 0))
mpsz_sp <- as_Spatial(mpsz_sf)
mpsz_spatialpoint <- as(mpsz_sp, "SpatialPolygons")

# Add count of facilities per subzone  into mpsz_demand
mpsz_demand <- mpsz_sf
mpsz_demand$`chas_count` <- lengths(st_intersects(mpsz_sf,chas_sf))
mpsz_demand$`eldercare_count` <- lengths(st_intersects(mpsz_sf,eldercare_sf))
mpsz_demand$`infocomm_count` <- lengths(st_intersects(mpsz_sf,infocomm_sf))

# Join HDB with MP14 Data
hdb_sf <- st_join(mpsz_sf, hdb_sf, join=st_intersects) %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N, REGION_N, elderly_proportion, Postcode, total_count.y, geometry) %>%
  rename(resident_count = total_count.y) %>%
  mutate(elderly_count = resident_count * elderly_proportion) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

#Finding area of each subzone
mpsz_demand$Area <- mpsz_demand %>%
    st_area()

#Calculating density of Chas, Eldercare, Infocomm and Elderly Pop
mpsz_demand <- mpsz_demand %>%
    mutate(`Elderly_Density` = (`elderly_count`/ Area) * 1000000) %>%
    mutate(`Chas_Density` = `elderly_count` / `chas_count`) %>%
    mutate(`Eldercare_Density` = `elderly_count` / `eldercare_count`) %>%
    mutate(`Infocomm_Density` = `elderly_count` / `infocomm_count`)

#Converting mpsz_demand to SP object
mpsz_demand_sp <- as_Spatial(mpsz_demand)
mpsz_demand_spatialpoint <- as(mpsz_demand_sp, "SpatialPolygons")

#reading OSM 
sg_osm <- read_osm(mpsz_spatialpoint, ext=1.3)

#Extracting unique planning areas
uniqPlanningAreas <- mpsz_demand[2]
uniqPlanningAreas <- st_set_geometry(uniqPlanningAreas, NULL)
uniqPlanningAreas <- unique(uniqPlanningAreas)

varPlnArea <- c(
    "TAMPINES",
    "OUTRAM",
    "BUKIT MERAH",
    "QUEENSTOWN",
    "MARINA EAST",
    "RIVER VALLEY",
    "DOWNTOWN CORE",
    "STRAITS VIEW",
    "MARINE PARADE",
    "ORCHARD",
    "ROCHOR",
    "KALLANG",
    "TANGLIN",
    "NEWTON",
    "CLEMENTI",
    "TUAS",
    "BEDOK",
    "PIONEER",
    "JURONG EAST",
    "BUKIT TIMAH",
    "NOVENA",
    "GEYLANG",
    "BOON LAY",
    "TOA PAYOH",
    "JURONG WEST",
    "BUKIT BATOK",
    "SERANGOON",
    "PAYA LEBAR",
    "BISHAN",
    "HOUGANG",
    "BUKIT PANJANG",
    "ANG MO KIO",
    "CHOA CHU KANG",
    "PASIR RIS",
    "CHANGI",
    "SENGKANG",
    "CHANGI BAY",
    "TENGAH",
    "PUNGGOL",
    "YISHUN",
    "MANDAI",
    "SELETAR",
    "WOODLANDS",
    "SEMBAWANG",
    "SIMPANG",
    "LIM CHU KANG"
)

varEdaSel <- c(
    "Elderly Density",
    "Elderly Count", 
    "Elderly Proportion"
)

sg_owin <- as(mpsz_sp, "owin")

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

boxmap <- function(vnam,df,legtitle=NA,mtitle,mult=1.5){
    var <- get.var(vnam,df)
    bb <- boxbreaks(var)
    tm_shape(df) +
        tm_fill(vnam,title=legtitle,breaks=bb,palette="-RdBu",
                labels = c("lower outlier", "< 25%", "25% - 50%", "50% - 75%","> 75%", "upper outlier")) +
        tm_borders() +
        tm_layout(title = mtitle, title.position = c("right","bottom"))
}

ui <- navbarPage("IS415 Team2",
           tabPanel("EDA",
                        column(12,
                               titlePanel("Spatial Point Pattern Analysis"),
                                   column(2,
                                          selectInput('edasel', 'Select Visualisation', choices = varEdaSel, selected = "Elderly Density"),
                                          checkboxGroupInput("cb_svc", "Select Services",
                                                             c("CHAS Clinics",
                                                               "Eldercare Centres",
                                                               "Silver infocomm"))
                                   ),
                                   column(5,
                                          tmapOutput("boxplot")
                                   ),
                                   column(5,
                                          tmapOutput("sdplot")
                                   )
                        ),
                        column(12), 
                        column(12,
                               column(2, 
                                      ),
                               
                               column(3,
                                      plotOutput("edaHistChas")
                               ),
                               column(3,
                                      plotOutput("edaHistElder")
                               ),
                               column(3,
                                      plotOutput("edaHistInfo")
                               )
                        )
                    ),
           tabPanel("Spatial Point Pattern",
                    column(12,
                           titlePanel("Kernal Density Plots"),
                           column(2,
                                  selectInput('planningarea', 'Select Planning Area', choices = varPlnArea, selected = "Tampines"),
                                  radioButtons("kcoptions", "View cross-k results",
                                               c("Yes","No"),
                                               selected = "No")
                                  
                           ),
                           column(3,
                                  tmapOutput("kdeplotchas") %>% withSpinner(color="#0dc5c1")
                                  
                           ),
                           column(3,
                                  tmapOutput("kdeploteldercare") %>% withSpinner(color="#0dc5c1")
                                  
                           ),
                           column(3,
                                  tmapOutput("kdeplotsilverinfo") %>% withSpinner(color="#0dc5c1")
                                  
                           ),
                           
                    ),
                    column(12), 
                    column(12,
                           column(2, 
                           ),
                           
                           column(3,
                                  plotOutput("kChas") %>% withSpinner(color="#0dc5c1")
                           ),
                           column(3,
                                  plotOutput("kEldercare") %>% withSpinner(color="#0dc5c1")
                           ),
                           column(3,
                                  plotOutput("kSilverinfo") %>% withSpinner(color="#0dc5c1")
                           ) 
                    )
           ),
           tabPanel("Accessibility", 
                    fixedRow(
                      column(12,
                             titlePanel("Accessibility Analysis"),
                             column(2,
                                    selectInput(inputId='facility', label='Select Facility', choices = c('Eldercare Centres', 'Silver Infocomm Junctions', 'CHAS Clinics'), selected='Eldercare Centres'),
                                    sliderInput(inputId='capacity', label='Select Capacity', min=1, max=150, value=50,round=TRUE), 
                                    sliderInput(inputId='distance', label='Select Distance Threshold', min=0.1, max=30, round=FALSE, value=1)
                                    
                             ),
                             column(8,
                                    tmapOutput("accplot") %>% withSpinner(color="#0dc5c1")
                                    
                             )
                             
                      )
                    ))
)


server <- function(input, output) {
  
  data <- eventReactive(input$kcoptions,{
    rnorm(1:100000)
  })

    output$boxplot <- renderTmap({
        if(input$edasel == "Elderly Density"){
            selected = "Elderly_Density"
            boxmap(selected, mpsz_demand, mtitle="Elderly Distribution by Density")
        }
        else if (input$edasel == "Elderly Count"){
            selected = "elderly_count"
            boxmap(selected, mpsz_demand, mtitle="Elderly Distribution by Count")
        }
        else{
          selected = 'elderly_proportion'
          boxmap(selected, mpsz_demand, mtitle='Elderly Distribution by Proportion')
        }
    })
    
    
    # Accessibility Maps
    
    output$accplot <- renderTmap({
      demand <- hdb_sf 
      temp_sf <- hdb_sf 
      bindingbox <- st_bbox(mpsz_sf)
      selected_osm <- read_osm(bindingbox, ext=1.1)
      
      if (input$facility == 'Eldercare Centres'){
        supply <- eldercare_sf %>% 
          mutate(capacity = input$capacity)
      }
      else if (input$facility == 'CHAS Clinics'){
        supply <- chas_sf %>% 
          mutate(capacity = input$capacity)
      }
      else{
        supply <- infocomm_sf %>% 
          mutate(capacity = input$capacity)
      }
    
      distmat = as.matrix(CreateDistMatrix(knownpts=demand, unknownpts=supply, longlat=FALSE)/1000)
      temp <- data.frame(ac(demand$elderly_count,supply$capacity, distmat, d0 = input$distance, power = 2, family = 'SAM'))
      colnames(temp) <- 'accSAM'
      temp <- tibble::as_tibble(temp)
      result_sf <- bind_cols(temp_sf, temp)
      
      # Generate the Map
      tm_shape(result_sf)+
        tm_borders(alpha=0.6)+
        tm_fill(col='accSAM', style='pretty')
      
    })
    
    
    
    #Supply and Demand of 3 different elder care facilities by density and elderly count
    output$sdplot <- renderTmap({
        if(input$edasel == "Elderly Density" & is.null(input$cb_svc)){
            selected = "Elderly_Density"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Density",
                        style = "jenks", 
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Density"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1)
        }
        
        else if(input$edasel == "Elderly Density" & "CHAS Clinics" %in% input$cb_svc & "Eldercare Centres" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
            selected = "Elderly_Density"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Density",
                        style = "jenks", 
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Density"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(chas_sf) + tm_dots("red") +
                tm_shape(eldercare_sf) + tm_dots("blue")+
                tm_shape(infocomm_sf) + tm_dots("green") 
        }
        
        else if(input$edasel == "Elderly Density" & "CHAS Clinics" %in% input$cb_svc & "Eldercare Centres" %in% input$cb_svc){
            selected = "Elderly_Density"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Density",
                        style = "jenks", 
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Density"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(chas_sf) + tm_dots("red") +
                tm_shape(eldercare_sf) + tm_dots("blue")
        }
        
        else if(input$edasel == "Elderly Density" & "CHAS Clinics" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
            selected = "Elderly_Density"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Density",
                        style = "jenks", 
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Density"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(chas_sf) + tm_dots("red") +
                tm_shape(infocomm_sf) + tm_dots("green") 
        }
        
        else if(input$edasel == "Elderly Density" & "Eldercare Centres" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
            selected = "Elderly_Density"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Density",
                        style = "jenks", 
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Density"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(eldercare_sf) + tm_dots("blue")+
                tm_shape(infocomm_sf) + tm_dots("green") 
        }
        
        
        else if(input$edasel == "Elderly Density" & "CHAS Clinics" %in% input$cb_svc){
            selected = "Elderly_Density"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Density",
                        style = "jenks", 
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Density"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(chas_sf) + tm_dots("red")
        }
        
        else if(input$edasel == "Elderly Density" & "Eldercare Centres" %in% input$cb_svc){
            selected = "Elderly_Density"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Density",
                        style = "jenks", 
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Density"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(eldercare_sf) + tm_dots("blue")
        }
        
        else if(input$edasel == "Elderly Density" & "Silver infocomm" %in% input$cb_svc){
            selected = "Elderly_Density"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Density",
                        style = "jenks", 
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Density"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(infocomm_sf) + tm_dots("green")
        }
        
        else if(input$edasel == "Elderly Count" & is.null(input$cb_svc)){
            selected = "elderly_count"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Population",
                        breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Population"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1)
        }
        
        else if(input$edasel == "Elderly Count" & "CHAS Clinics" %in% input$cb_svc & "Eldercare Centres" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
            selected = "elderly_count"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Population",
                        breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Population"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(chas_sf) + tm_dots("red") +
                tm_shape(eldercare_sf) + tm_dots("blue")+
                tm_shape(infocomm_sf) + tm_dots("green") 
        }
        
        else if(input$edasel == "Elderly Count" & "CHAS Clinics" %in% input$cb_svc & "Eldercare Centres" %in% input$cb_svc){
            selected = "elderly_count"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Population",
                        breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Population"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(chas_sf) + tm_dots("red") +
                tm_shape(eldercare_sf) + tm_dots("blue")
        }
        
        else if(input$edasel == "Elderly Count" & "CHAS Clinics" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
            selected = "elderly_count"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Population",
                        breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Population"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(chas_sf) + tm_dots("red") +
                tm_shape(infocomm_sf) + tm_dots("green")
        }
        
        else if(input$edasel == "Elderly Count" & "Eldercare Centres" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
            selected = "elderly_count"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Population",
                        breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Population"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(eldercare_sf) + tm_dots("blue")+
                tm_shape(infocomm_sf) + tm_dots("green") 
        }
        
        else if(input$edasel == "Elderly Count" & "CHAS Clinics" %in% input$cb_svc){
            selected = "elderly_count"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Population",
                        breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Population"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(chas_sf) + tm_dots("red")
        }
        
        else if(input$edasel == "Elderly Count" & "Eldercare Centres" %in% input$cb_svc){
            selected = "elderly_count"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Population",
                        breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Population"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(eldercare_sf) + tm_dots("blue")
        }
        
        else if(input$edasel == "Elderly Count" & "Silver infocomm" %in% input$cb_svc){
            selected = "elderly_count"
            
            tm_shape(mpsz_demand) + 
                tm_fill(c(selected),
                        title = "Elderly Population",
                        breaks = c(0, 2500 ,5000, 7500, 10000, 12500, 15000, 17500, 20000),
                        palette = "Blues",
                        popup.vars=c("Subzone Name"="SUBZONE_N", 
                                     "Planning Area Name"="PLN_AREA_N", 
                                     "Elderly Population"=selected),
                        showNA = FALSE) +
                tm_borders(lwd = 0.1,  alpha = 1) +
                tm_shape(infocomm_sf) + tm_dots("green") 
        }
      
        else if(input$edasel == "Elderly Proportion" & is.null(input$cb_svc)){
          selected = "elderly_proportion"
          
          tm_shape(mpsz_demand) + 
            tm_fill(c(selected),
                    title = "Elderly Proportion",
                    style = "jenks", 
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Proportion"=selected),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1)
        }
        
        else if(input$edasel == "Elderly Proportion" & "CHAS Clinics" %in% input$cb_svc & "Eldercare Centres" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
          selected = "elderly_proportion"
          
          tm_shape(mpsz_demand) + 
            tm_fill(c(selected),
                    title = "Elderly Proportion",
                    style = "jenks", 
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Proportion"=selected),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1) +
            tm_shape(chas_sf) + tm_dots("red") +
            tm_shape(eldercare_sf) + tm_dots("blue")+
            tm_shape(infocomm_sf) + tm_dots("green") 
        }
        
        else if(input$edasel == "Elderly Proportion" & "CHAS Clinics" %in% input$cb_svc & "Eldercare Centres" %in% input$cb_svc){
          selected = "elderly_proportion"
          
          tm_shape(mpsz_demand) + 
            tm_fill(c(selected),
                    title = "Elderly Proportion",
                    style = "jenks", 
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Proportion"=selected),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1) +
            tm_shape(chas_sf) + tm_dots("red") +
            tm_shape(eldercare_sf) + tm_dots("blue")
        }
        
        else if(input$edasel == "Elderly Proportion" & "CHAS Clinics" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
          selected = "elderly_proportion"
          
          tm_shape(mpsz_demand) + 
            tm_fill(c(selected),
                    title = "Elderly Proportion",
                    style = "jenks", 
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Proportion"=selected),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1) +
            tm_shape(chas_sf) + tm_dots("red") +
            tm_shape(infocomm_sf) + tm_dots("green") 
        }
        
        else if(input$edasel == "Elderly Proportion" & "Eldercare Centres" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
          selected = "elderly_proportion"
          
          tm_shape(mpsz_demand) + 
            tm_fill(c(selected),
                    title = "Elderly Proportion",
                    style = "jenks", 
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Proportion"=selected),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1) +
            tm_shape(eldercare_sf) + tm_dots("blue")+
            tm_shape(infocomm_sf) + tm_dots("green") 
        }
        
        
        else if(input$edasel == "Elderly Proportion" & "CHAS Clinics" %in% input$cb_svc){
          selected = "elderly_proportion"
          
          tm_shape(mpsz_demand) + 
            tm_fill(c(selected),
                    title = "Elderly Proportion",
                    style = "jenks", 
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Proportion"=selected),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1) +
            tm_shape(chas_sf) + tm_dots("red")
        }
        
        else if(input$edasel == "Elderly Proportion" & "Eldercare Centres" %in% input$cb_svc){
          selected = "elderly_proportion"
          
          tm_shape(mpsz_demand) + 
            tm_fill(c(selected),
                    title = "Elderly Proportion",
                    style = "jenks", 
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Proportion"=selected),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1) +
            tm_shape(eldercare_sf) + tm_dots("blue")
        }
        
        else if(input$edasel == "Elderly Proportion" & "Silver infocomm" %in% input$cb_svc){
          selected = "elderly_proportion"
          
          tm_shape(mpsz_demand) + 
            tm_fill(c(selected),
                    title = "Elderly Proportion",
                    style = "jenks", 
                    palette = "Blues",
                    popup.vars=c("Subzone Name"="SUBZONE_N", 
                                 "Planning Area Name"="PLN_AREA_N", 
                                 "Elderly Proportion"=selected),
                    showNA = FALSE) +
            tm_borders(lwd = 0.1,  alpha = 1) +
            tm_shape(infocomm_sf) + tm_dots("green")
        }

    })
    
    #values <- reactiveValues()

    output$edaHistChas <- renderPlot({
        if("CHAS Clinics" %in% input$cb_svc){
            ggplot(data=mpsz_demand, 
                   aes(x= as.numeric(`Chas_Density`)))+
                geom_histogram(bins=20, 
                               color="black", fill="light blue")
        }
    })
    
    output$edaHistInfo <- renderPlot({
        if("Silver infocomm" %in% input$cb_svc){
            ggplot(data=mpsz_demand, 
                   aes(x= as.numeric(`Infocomm_Density`)))+
                geom_histogram(bins=20, 
                               color="black", fill="light blue")
        }
    })
    
    output$edaHistElder <- renderPlot({
        if("Eldercare Centres" %in% input$cb_svc){
            ggplot(data=mpsz_demand, 
                   aes(x= as.numeric(`Eldercare_Density`)))+
                geom_histogram(bins=20, 
                               color="black", fill="light blue")
        }
    })
    
    output$kdeplotchas <- renderTmap({
        pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
        bindingbox <- st_bbox(mpsz_sf %>% filter(PLN_AREA_N == input$planningarea,))

        owin <- as(pln_area_selected, "owin")
        spatialpoint <- as(pln_area_selected, "SpatialPolygons")
        chas_ppp <- as(chas_spatialpoint, "ppp")
        chas_ppp_jit <- rjitter(chas_ppp, retry=TRUE, nsim=1, drop=TRUE)

        ppp_chas <- chas_ppp_jit[owin]

        selected_osm <- read_osm(bindingbox, ext=1.1)
        
        msgtext <- "KDE Map"
        
        ppp.km <- rescale(ppp_chas, 1000, "km")
        
        kde <- density(ppp.km, sigma=0.20, edge=TRUE, kernel="gaussian")
        
        gridded_kde_bw <- as.SpatialGridDataFrame.im(kde)
        
        kde_bw_raster <- raster(gridded_kde_bw)
        
        projection(kde_bw_raster) <- crs("+init=EPSG:3414 +datum=WGS84 +units=km")
        
        # Plot kernel density map on openstreetmap
        tm_shape(selected_osm)+ 
            tm_layout(legend.outside = TRUE, title="msg_text")+
            tm_rgb()+
            tm_shape(pln_area_selected)+
            tm_borders(col = "darkblue", lwd = 2, lty="longdash")+
            tm_shape(kde_bw_raster) + 
            tm_raster("v", alpha=0.5,  
                      palette = "YlOrRd")
        
    }) 
    
      output$kChas <- renderPlot({
        if(input$kcoptions == "Yes"){
          pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
          
          owin <- as(pln_area_selected, "owin")
          #spatialpoint <- as(pln_area_selected, "SpatialPolygons")
          chas_ppp <- as(chas_spatialpoint, "ppp")
          chas_ppp_jit <- rjitter(chas_ppp, retry=TRUE, nsim=1, drop=TRUE)
          
          ppp_chas <- chas_ppp_jit[owin]
          
          K_chas = Kest(ppp_chas, correction = "Ripley")
          K_chas.csr <- envelope(ppp_chas, Kest, nsim = 99, rank = 1, glocal=TRUE)
          
          plot(K_chas, . - r ~ r, 
               xlab="d", ylab="L(d)-r", 
               main="Chas Clinics")
        }
      })
    
    
    output$kdeploteldercare <- renderTmap({
        pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
        bindingbox <- st_bbox(mpsz_sf %>% filter(PLN_AREA_N == input$planningarea,))
        
        owin <- as(pln_area_selected, "owin")
        spatialpoint <- as(pln_area_selected, "SpatialPolygons")
        elder_ppp <- as(eldercare_spatialpoint, "ppp")
        elder_ppp_jit <- rjitter(elder_ppp, retry=TRUE, nsim=1, drop=TRUE)
        
        ppp_elder<- elder_ppp_jit[owin]
        
        selected_osm <- read_osm(bindingbox, ext=1.1)
        
        msgtext <- "KDE Map"
        
        ppp.km <- rescale(ppp_elder, 1000, "km")
        
        kde <- density(ppp.km, sigma=0.20, edge=TRUE, kernel="gaussian")
        
        gridded_kde_bw <- as.SpatialGridDataFrame.im(kde)
        
        kde_bw_raster <- raster(gridded_kde_bw)
        
        projection(kde_bw_raster) <- crs("+init=EPSG:3414 +datum=WGS84 +units=km")
        
        # Plot kernel density map on openstreetmap
        tm_shape(selected_osm)+ 
            tm_layout(legend.outside = TRUE, title="msg_text")+
            tm_rgb()+
            tm_shape(pln_area_selected)+
            tm_borders(col = "darkblue", lwd = 2, lty="longdash")+
            tm_shape(kde_bw_raster) + 
            tm_raster("v", alpha=0.5,  
                      palette = "YlOrRd")
        
    }) 
    
    output$kEldercare <- renderPlot({
      if(input$kcoptions == "Yes"){
        pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
        
        owin <- as(pln_area_selected, "owin")
        #spatialpoint <- as(pln_area_selected, "SpatialPolygons")
        elder_ppp <- as(eldercare_spatialpoint, "ppp")
        elder_ppp_jit <- rjitter(elder_ppp, retry=TRUE, nsim=1, drop=TRUE)
        
        ppp_elder<- elder_ppp_jit[owin]
        
        K_elder = Kest(ppp_elder, correction = "Ripley")
        K_elder.csr <- envelope(ppp_elder, Kest, nsim = 99, rank = 1, glocal=TRUE)
        
        plot(K_elder, . - r ~ r, 
             xlab="d", ylab="L(d)-r", 
             main="Eldercare Facilities")
      }
    })
    
    output$kdeplotsilverinfo <- renderTmap({
        pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
        bindingbox <- st_bbox(mpsz_sf %>% filter(PLN_AREA_N == input$planningarea,))
        
        owin <- as(pln_area_selected, "owin")
        spatialpoint <- as(pln_area_selected, "SpatialPolygons")
        silver_ppp <- as(infocomm_spatialpoint, "ppp")
        silver_ppp_jit <- rjitter(silver_ppp, retry=TRUE, nsim=1, drop=TRUE)
        
        ppp_silver <- silver_ppp_jit[owin]
        
        selected_osm <- read_osm(bindingbox, ext=1.1)
        
        msgtext <- "KDE Map"
        
        ppp.km <- rescale(ppp_silver, 1000, "km")
        
        kde <- density(ppp.km, sigma=0.20, edge=TRUE, kernel="gaussian")
        
        gridded_kde_bw <- as.SpatialGridDataFrame.im(kde)
        
        kde_bw_raster <- raster(gridded_kde_bw)
        
        projection(kde_bw_raster) <- crs("+init=EPSG:3414 +datum=WGS84 +units=km")
        
        # Plot kernel density map on openstreetmap
        tm_shape(selected_osm)+ 
            tm_layout(legend.outside = TRUE, title="msg_text")+
            tm_rgb()+
            tm_shape(pln_area_selected)+
            tm_borders(col = "darkblue", lwd = 2, lty="longdash")+
            tm_shape(kde_bw_raster) + 
            tm_raster("v", alpha=0.5,  
                      palette = "YlOrRd")
        
    }) 
    
    output$kSilverinfo <- renderPlot({
      if(input$kcoptions == "Yes"){
        pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
        
        owin <- as(pln_area_selected, "owin")
        #spatialpoint <- as(pln_area_selected, "SpatialPolygons")
        silver_ppp <- as(infocomm_spatialpoint, "ppp")
        silver_ppp_jit <- rjitter(silver_ppp, retry=TRUE, nsim=1, drop=TRUE)
        
        ppp_silver <- silver_ppp_jit[owin]
        
        K_info = Kest(ppp_silver, correction = "Ripley")
        K_info.csr <- envelope(ppp_silver, Kest, nsim = 99, rank = 1, glocal=TRUE)
        
        plot(K_info, . - r ~ r, 
             xlab="d", ylab="L(d)-r", 
             main="Silver infocomm Centres")
      }
    })
    
    output$text <- renderText({
        visFun <- renderText({input$planningarea})
        visFun()
    })
}

    output$hotspot <- renderTmap({
      dnb <- dnearneigh(coordinates(hunan), 0, 85, longlat = TRUE)
      
    })
# Run the application 
shinyApp(ui = ui, server = server)
