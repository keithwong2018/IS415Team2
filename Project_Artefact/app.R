#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rsconnect::deployApp('C:/Users/keith/Documents/GitHub/IS415Team2/Project_Artefact/app')

library(rgdal)
library(sf)
library(tmap)
library(tidyverse)
library(rgeos)
library(maptools)
library(raster)
library(spatstat)
library(tmaptools)
library(spdep)
library(OpenStreetMap)
library(ggpubr)
library(SpatialPosition)
library(SpatialAcc)
library(dplyr)
library(shinycssloaders)
library(plotly)
library(shinythemes)
library(readr)
library(leaflet.providers)

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
hdb_mpsz <- st_join(mpsz_sf, hdb_sf, join=st_intersects) %>%
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
    "RIVER VALLEY",
    "DOWNTOWN CORE",
    "MARINE PARADE",
    "ORCHARD",
    "ROCHOR",
    "KALLANG",
    "TANGLIN",
    "NEWTON",
    "CLEMENTI",
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
    "PUNGGOL",
    "YISHUN",
    "SELETAR",
    "WOODLANDS",
    "SEMBAWANG",
    "SIMPANG"
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

ui <- fluidPage(theme=shinytheme('spacelab'),
        
          # Navigation Bar
          navbarPage("Silver is the New Black", fluid=TRUE, windowTitle='Silver is the New Black', selected='EDA', 
                     
                     tabPanel("Home", value='home', fluid=TRUE, icon=icon('home')
                              ), 
                     
                     tabPanel("EDA", value='eda', fluid=TRUE, icon=icon('search'), 
                              sidebarLayout(position='left', fluid=TRUE, 
                                            sidebarPanel(width=3, fluid=TRUE, 
                                                         conditionalPanel(
                                                           'input.EDAset == "Outlier Analysis"', 
                                                           selectInput(inputId='boxeda', 
                                                                       label='Population Variable', 
                                                                       choices=varEdaSel,
                                                                       selected='Elderly Proportion'), 
                                                           # insert description here
                                                         ), 
                                                         conditionalPanel(
                                                           'input.EDAset == "Distribution Analysis"', 
                                                           selectInput(inputId='edasel', 
                                                                       label='Population Variable', 
                                                                       choices=varEdaSel, 
                                                                       selected='Elderly Density'), 
                                                           checkboxGroupInput(inputId='cb_svc', 
                                                                              label='Facility', 
                                                                              choices=c('Eldercare Services'='Eldercare Centres', 
                                                                                        'Silver Infocomm Junctions'='Silver infocomm',
                                                                                        'CHAS Clinics'='CHAS Clinics'
                                                                                        ))
                                                         )
                                                ), 
                              
                                            mainPanel(width=9, fluid=TRUE, 
                                                      tabsetPanel(
                                                        id='EDAset', 
                                                        tabPanel('Distribution Analysis', 
                                                                 fluidRow(column(9, tmapOutput('sdplot'))), 
                                                                 fluidRow(), 
                                                                 fluidRow(
                                                                   column(3, plotOutput('edaHistElder')), 
                                                                   column(3, plotOutput('edaHistInfo')), 
                                                                   column(3, plotOutput('edaHistChas'))
                                                                 )
                                                        ), 
                                                        tabPanel('Outlier Analysis', 
                                                                 fluidRow(column(9, tmapOutput("boxmap"))), 
                                                                 fluidRow(column(9, plotOutput('boxplot')))
                                                                 )
                                                        
                                                      )
                                            )
                          )
                    ), 
                    
                    tabPanel('SPPA', value='sppa', fluid=TRUE, icon=icon('globe-americas'), 
                             sidebarLayout(position='left', fluid=TRUE, 
                                           sidebarPanel(width=3, fluid=TRUE, 
                                                        conditionalPanel(
                                                          'input.SPPAset == "Overview"', 
                                                          selectInput(inputId='overviewfacility', 
                                                                      label= "Facility", 
                                                                      choices=c('Eldercare Services', 'Silver Infocomm Junctions', 'CHAS Clinics'), 
                                                                      selected='Eldercare Services')
                                                        ), 
                                                        conditionalPanel(
                                                          'input.SPPAset == "Quadrat Analysis" | input.SPPAset == "KDE Maps" | input.SPPAset == "K-Cross Analysis" | input.SPPAset == "Hotspots & Coldspots"', 
                                                          selectInput(inputId='planningarea',
                                                                      label='Planning Area',
                                                                      choices=varPlnArea, 
                                                                      selected='Tampines')
                                                        )
                                                        ), 
                                           mainPanel(width=9, fluid=TRUE, 
                                                     tabsetPanel(
                                                       id='SPPAset',
                                                       tabPanel('Quadrat Analysis', 
                                                                column(3, plotOutput('quadploteldercare') %>% withSpinner(color='#0dc5c1')), 
                                                                column(3, plotOutput('quadplotsilverinfo') %>% withSpinner(color='#0dc5c1')),
                                                                column(3, plotOutput('quadplotchas') %>% withSpinner(color='#0dc5c1'))
                                                        ), 
                                                       tabPanel('KDE Maps', 
                                                                column(3, tmapOutput('kdeploteldercare') %>% withSpinner(color='#0dc5c1')),
                                                                column(3, tmapOutput('kdeplotsilverinfo') %>% withSpinner(color='#0dc5c1')),
                                                                column(3, tmapOutput('kdeplotchas') %>% withSpinner(color='#0dc5c1'))
                                                       ), 
                                                       tabPanel('K-Cross Analysis', 
                                                                column(3, plotOutput('kEldercare') %>% withSpinner(color='#0dc5c1')),
                                                                column(3, plotOutput('kSilverinfo') %>% withSpinner(color='#0dc5c1')),
                                                                column(3, plotOutput('kChas') %>% withSpinner(color='#0dc5c1'))
                                                      ), 
                                                      tabPanel('Hotspots & Coldspots', 
                                                               column(3, tmapOutput('hotspotelder') %>% withSpinner(color='#0dc5c1')),
                                                               column(3, tmapOutput('hotspotinfo') %>% withSpinner(color='#0dc5c1')),
                                                               column(3, tmapOutput('hotspotchas') %>% withSpinner(color='#0dc5c1'))
                                                      )
                                                     )
                                                     )
                                           )
                             ), 
                    tabPanel('Geographical Accessibility', value='accessibility', fluid=TRUE, icon=icon('globe-asia'), 
                             sidebarLayout(position='left', fluid=TRUE, 
                                           sidebarPanel(width=3, fluid=TRUE, 
                                                        selectInput(inputId='facility', 
                                                                    label='Facility', 
                                                                    choices = c('Eldercare Services', 'Silver Infocomm Junctions', 'CHAS Clinics'), 
                                                                    selected='Eldercare Services'),
                                                        selectInput(inputId='accmeasure', 
                                                                    label='Accessibility Measure', 
                                                                    choices = c('SAM', 'Hansen'), 
                                                                    selected='Hansen'), 
                                                        sliderInput(inputId='capacity', 
                                                                    label='Capacity', 
                                                                    min=1, 
                                                                    max=120, 
                                                                    value=50,
                                                                    round=TRUE), 
                                                        sliderInput(inputId='distance', 
                                                                    label='Distance Threshold', 
                                                                    min=0.1, 
                                                                    max=40, 
                                                                    round=FALSE, 
                                                                    value=1)
                                                        ), 
                                           mainPanel(width=9, fluid=TRUE, 
                                                     id='accessibility', 
                                                     fluidRow(column(9, tmapOutput("accsubzoneplot") %>% withSpinner(color="#0dc5c1"))), 
                                                     fluidRow(column(9, tmapOutput("acchdbplot") %>% withSpinner(color="#0dc5c1")))
                                                     )
                                                     
                                           )
                             )
                    )
          )
           

           
           
           



server <- function(input, output) {
  
  data <- eventReactive(input$kcoptions,{
    rnorm(1:100000)
  })
  
  # Boxmap
    output$boxmap <- renderTmap({
        if(input$boxeda == "Elderly Density"){
            selected = "Elderly_Density"
            boxmap(selected, mpsz_demand, mtitle="Elderly Distribution by Density")
        }
        else if (input$boxeda == "Elderly Count"){
            selected = "elderly_count"
            boxmap(selected, mpsz_demand, mtitle="Elderly Distribution by Count")
        }
        else{
          selected = 'elderly_proportion'
          boxmap(selected, mpsz_demand, mtitle='Elderly Distribution by Proportion')
        }
    })
    
    # Box plots
    output$boxplot <- renderPlot({
      if (input$boxeda == 'Elderly Density'){
        selected='Elderly_Density'
        boxplot(mpsz_demand$selected, 
                main = 'Distribution of Elderly Density', 
                xlab = 'Elderly Density', 
                xlim = max(mpsz_demand$selected),
                col = 'orange', 
                border = 'brown', 
                horizontal = TRUE, 
                notch = TRUE)
      }
      else if (input$boxeda == 'Elderly Count'){
        selected='elderly_count'
        boxplot(mpsz_demand$selected, 
                main = 'Distribution of Elderly Count', 
                xlab = 'Elderly Count', 
                xlim = max(mpsz_demand$selected),
                col = 'orange', 
                border = 'brown', 
                horizontal = TRUE, 
                notch = TRUE)
      }
      else {
        selected='elderly_proportion'
        boxplot(mpsz_demand$selected, 
                main = 'Distribution of Elderly Proportion', 
                xlab = 'Elderly Proportion', 
                xlim = 1,
                col = 'orange', 
                border = 'brown', 
                horizontal = TRUE, 
                notch = TRUE) 
      }
      
    })
    
    
    # Accessibility Maps
    output$accsubzoneplot <- renderTmap({
      demand <- hdb_mpsz
      temp_sf <- hdb_mpsz 
      bindingbox <- st_bbox(mpsz_sf)
      selected_osm <- read_osm(bindingbox, ext=1.1)
      
      if (input$facility == 'Eldercare Services'){
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
      temp <- data.frame(ac(demand$elderly_count,supply$capacity, distmat, d0 = input$distance, power = 2, family = input$accmeasure))
      colnames(temp) <- 'acc'
      temp <- tibble::as_tibble(temp)
      result_sf <- bind_cols(temp_sf, temp)
      
      # Generate the Map
      tm_shape(result_sf)+
        tm_borders(alpha=0.6)+
        tm_fill(col='acc', style='pretty') 
      
    })
    
    output$acchdbplot <- renderTmap({
      demand <- hdb_sf 
      temp_sf <- hdb_sf 
      bindingbox <- st_bbox(mpsz_sf)
      selected_osm <- read_osm(bindingbox, ext=1.1)
      
      if (input$facility == 'Eldercare Services'){
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
      temp <- data.frame(ac(demand$elderly_count,supply$capacity, distmat, d0 = input$distance, power = 2, family = input$accmeasure))
      colnames(temp) <- 'acc'
      temp <- tibble::as_tibble(temp)
      result_sf <- bind_cols(temp_sf, temp)
      
      # Generate the Map
      if (input$facility == 'Eldercare Centres'){
        tm_shape(eldercare_sf) + 
          tm_dots('blue') + 
        tm_shape(result_sf) + 
          tm_bubbles(col='acc', n=5, style='quantile', size=0.005, border.col='black', border.lwd=1)
      }
      else if (input$facility == 'CHAS Clinics'){
        tm_shape(chas_sf) + 
          tm_dots('red') + 
        tm_shape(result_sf) + 
          tm_bubbles(col='acc', n=5, style='quantile', size=0.005, border.col='black', border.lwd=1)
      }
      else{
        tm_shape(infocomm_sf) + 
          tm_dots('green') + 
        tm_shape(result_sf) + 
          tm_bubbles(col='acc', n=5, style='quantile', size=0.005, border.col='black', border.lwd=1)
      }
      
    })
    
    output$cda <- renderPlot({
      demand <- hdb_sf 
      temp_sf <- hdb_sf 
      bindingbox <- st_bbox(mpsz_sf)
      selected_osm <- read_osm(bindingbox, ext=1.1)
      
      if (input$facility == 'Eldercare Services'){
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
      temp <- data.frame(ac(demand$elderly_count,supply$capacity, distmat, d0 = input$distance, power = 2, family = input$accmeasure))
      colnames(temp) <- 'acc'
      temp <- tibble::as_tibble(temp)
      result_sf <- bind_cols(temp_sf, temp)
      result_sf <- st_join(result_sf, mpsz_sf, join=st_intersects)
      
      result_sf$log_acc <- log(result_sf$acc)
      ggbetweenstats(data=result_sf, 
                     x=REGION_N, 
                     y=log_acc, 
                     pairwise.comparisons=TRUE, 
                     p.adjust.method='fdr', 
                     title='Accessibility Values by HDB location & by Region')
      
    })
    
    # Histograms
    output$plotlychas <- renderPlotly({
      if("CHAS Clinics" %in% input$cb_svc){
        chasplot <- plot_ly(x = mpsz_demand$`Chas_Density`, type = "histogram", fill="red")
        chasplot <- chasplot %>% layout(title = 'Demand/Supply of CHAS Clinics',
                                        xaxis = list(title = 'Ratio of Eldertly to CHAS Clinics',
                                                     zeroline = TRUE,
                                                     range = c(0, 3500)),
                                        yaxis = list(title = 'Count',
                                                     range = c(0,50)))
        chasplot
      }
    })
    
    output$plotlyinfo <- renderPlotly({
      if("Silver infocomm" %in% input$cb_svc){
        infoplot <- plot_ly(x = mpsz_demand$`Infocomm_Density`, type = "histogram", fill="green")
        infoplot <- infoplot %>% layout(title = 'Demand/Supply of Silver Infocomm Junctions',
                                        xaxis = list(title = 'Ratio of Elderly to Silver Infocomm Junctions',
                                                     zeroline = TRUE,
                                                     range = c(0, 11000)),
                                        yaxis = list(title = 'Count',
                                                     range = c(0,12)))
        infoplot
      }
    })
    
    output$plotlyelder <- renderPlotly({
      if("Eldercare Centres" %in% input$cb_svc){
        elderplot <- plot_ly(x = mpsz_demand$`Eldercare_Density`, type = "histogram", fill="blue")
        elderplot <- elderplot %>% layout(title = 'Demand/Supply of Eldercare Services',
                                          xaxis = list(title = 'Ratio of Elderly to Eldercare Services',
                                                       zeroline = TRUE,
                                                       range = c(0, 8000)),
                                          yaxis = list(title = 'Count',
                                                       range = c(0,10)))
        elderplot
      }
    })
    
    
    
    output$edaHistChas <- renderPlot({
      if("CHAS Clinics" %in% input$cb_svc){
        ggplot(data=mpsz_demand,
               aes(x= as.numeric(`Chas_Density`)))+
          geom_histogram(bins=20, 
                         color="black", fill="red")+ 
          labs(x='Ratio of Elderly to No. of CHAS Clinics', 
               y='Count')
      }
    })
    
    
    
    output$edaHistInfo <- renderPlot({
      if("Silver infocomm" %in% input$cb_svc){
        ggplot(data=mpsz_demand, 
               aes(x= as.numeric(`Infocomm_Density`)))+
          geom_histogram(bins=20, 
                         color="black", fill="green")+
          labs(x='Ratio of Elderly to No. of Silver Infocomm Junctions', 
               y='Count')
      }
    })
    
    output$edaHistElder <- renderPlot({
      if("Eldercare Centres" %in% input$cb_svc){
        ggplot(data=mpsz_demand, 
               aes(x= as.numeric(`Eldercare_Density`)))+
          geom_histogram(bins=20, 
                         color="black", fill="blue")+ 
          labs(x='Ratio of Elderly to No. of Eldercare Centres', 
               y='Count')
      }
    })
    
    
    # Overview Maps
    output$kdeplotsg <- renderTmap({
      bindingbox <- st_bbox(sg)
      owin <- as(sg, "owin")
      spatialpoint <- as(sg, "SpatialPolygons")
      
      if (input$overviewfacility == 'CHAS Clinics'){
        chas_ppp <- as(chas_spatialpoint, "ppp")
        chas_ppp_jit <- rjitter(chas_ppp, retry=TRUE, nsim=1, drop=TRUE)
        ppp <- chas_ppp_jit[owin]
      }
      
      else if (input$overviewfacility == 'Eldercare Services'){
        elder_ppp <- as(eldercare_spatialpoint, "ppp")
        elder_ppp_jit <- rjitter(elder_ppp, retry=TRUE, nsim=1, drop=TRUE)
        ppp <- elder_ppp_jit[owin]
      }
      
      else{
        silver_ppp <- as(infocomm_spatialpoint, "ppp")
        silver_ppp_jit <- rjitter(silver_ppp, retry=TRUE, nsim=1, drop=TRUE)
        ppp <- silver_ppp_jit[owin]
      }
      
      selected_osm <- read_osm(bindingbox, ext=1.1)
      msgtext <- "KDE Map"
      ppp.km <- rescale(ppp, 1000, "km")
      kde <- density(ppp.km, sigma=bw.diggle, edge=TRUE, kernel="gaussian")
      gridded_kde_bw <- as.SpatialGridDataFrame.im(kde)
      kde_bw_raster <- raster(gridded_kde_bw)
      projection(kde_bw_raster) <- crs("+init=EPSG:3414 +datum=WGS84 +units=km")
      
      tm_shape(selected_osm)+ 
        tm_layout(legend.outside = TRUE, title="msg_text")+
        tm_rgb()+
        tm_shape(kde_bw_raster) + 
        tm_raster("v", alpha=0.5,  
                  palette = "YlOrRd")
      
    }) 
    
    
    output$quadplotsg <- renderPlot({
      owin <- as(sg, "owin")
      spatialpoint <- as(sg, "SpatialPolygons")
      
      if (input$overviewfacility == 'CHAS Clinics'){
        chas_ppp <- as(chas_spatialpoint, "ppp")
        chas_ppp_jit <- rjitter(chas_ppp, retry=TRUE, nsim=1, drop=TRUE)
        ppp <- chas_ppp_jit[owin]
      }
      
      else if (input$overviewfacility == 'Eldercare Services'){
        elder_ppp <- as(eldercare_spatialpoint, "ppp")
        elder_ppp_jit <- rjitter(elder_ppp, retry=TRUE, nsim=1, drop=TRUE)
        ppp <- elder_ppp_jit[owin]
      }
      
      else{
        silver_ppp <- as(infocomm_spatialpoint, "ppp")
        silver_ppp_jit <- rjitter(silver_ppp, retry=TRUE, nsim=1, drop=TRUE)
        ppp <- silver_ppp_jit[owin]
      }
      
      msgtext <- "Quadrat Map"
      ppp.km <- rescale(ppp, 1000, "km")
      
      qt <- quadrat.test(ppp.km, nx=20, ny=15)
      plot(ppp.km)
      plot(qt, add=TRUE, cex=.1)
    }) 
    
    output$hotspotsg <- renderTmap({
      
      mpsz_demand_selected <- mpsz_demand 
      mpsz_demand_selected_sp <- as_Spatial(mpsz_demand_selected)
      
      coords <- coordinates(mpsz_demand_selected_sp)
      k1 <- knn2nb(knearneigh(coords))
      k1dists <- unlist(nbdists(k1, coords, longlat=FALSE))
      maxdist <- ceiling(max(k1dists))
      dnb <- dnearneigh(coordinates(mpsz_demand_selected_sp), 0, maxdist, longlat = FALSE)
      dnb_lw <- nb2listw(dnb, style = 'B')
      fips <- order(mpsz_demand_selected_sp$SUBZONE_N)
      
      if (input$overviewfacility == 'Eldercare Services'){
        gi.fixed <- localG(mpsz_demand_selected_sp$eldercare_count, dnb_lw)
      }
      else if (input$overviewfacility == 'CHAS Clinics'){
        gi.fixed <- localG(mpsz_demand_selected_sp$chas_count, dnb_lw)
      }
      else{
        gi.fixed <- localG(mpsz_demand_selected_sp$infocomm_count, dnb_lw)
      }
      
      
      result.gi <- cbind(mpsz_demand_selected_sp, as.matrix(gi.fixed))
      names(result.gi)[15] <- "gstat"
      
      tm_shape(result.gi) +
        tm_fill(col = "gstat", 
                style = "pretty",
                palette="-RdBu",
                title = "local Gi") +
        tm_borders(alpha = 0.5)
    })
    
    
    # KDE Maps
    
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
      
      kde <- density(ppp.km, sigma=bw.diggle, edge=TRUE, kernel="gaussian")
      
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
      
      kde <- density(ppp.km, sigma=bw.diggle, edge=TRUE, kernel="gaussian")
      
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
      
      kde <- density(ppp.km, sigma=bw.diggle, edge=TRUE, kernel="gaussian")
      
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
    
    
    # K-Cross Analysis
    output$kChas <- renderPlot({
      
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
      
    })
    
    
    output$kSilverinfo <- renderPlot({
      
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
      
    })
    
    output$kEldercare <- renderPlot({
      
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
      
    })
    
    output$text <- renderText({
      #selecting planning area via user input
      mpsz_demand_selected <- mpsz_demand %>%
        filter(PLN_AREA_N == input$sel_plnarea)
      
      #coverting to sp polygons
      mpsz_demand_selected_sp <- as_Spatial(mpsz_demand_selected)
      
      coords <- coordinates(mpsz_demand_selected_sp)
      k1 <- knn2nb(knearneigh(coords))
      k1dists <- unlist(nbdists(k1, coords, longlat=FALSE))
      maxdist <- ceiling(max(k1dists))
      
      #Calculating d fixed-distance weight matrix
      dnb <- dnearneigh(coordinates(mpsz_demand_selected_sp), 0, maxdist, longlat = FALSE)
      
      #plot(mpsz_demand_selected_sp, border = 'lightgrey')
      #plot(dnb, coordinates(mpsz_demand_selected_sp), add=TRUE, col='red')
      
      dnb_lw <- nb2listw(dnb, style = 'B')
      
      fips <- order(mpsz_demand_selected_sp$SUBZONE_N)
      gi.fixed <- localG(mpsz_demand_selected_sp$eldercare_count, dnb_lw)
      gi.fixed
      
      elder.gi <- cbind(mpsz_demand_selected_sp, as.matrix(gi.fixed))
      print(names(elder.gi)[15])
    })
    
    output$plnAreatest <- renderPlot({
      mpsz_demand_selected <- mpsz_demand %>%
        filter(PLN_AREA_N == input$sel_plnarea)
      
      mpsz_demand_selected_sp <- as_Spatial(mpsz_demand_selected)
      
      coords <- coordinates(mpsz_demand_selected_sp)
      k1 <- knn2nb(knearneigh(coords))
      k1dists <- unlist(nbdists(k1, coords, longlat=FALSE))
      maxdist <- ceiling(max(k1dists))
      
      dnb <- dnearneigh(coordinates(mpsz_demand_selected_sp), 0, maxdist, longlat = FALSE)
      
      plot(mpsz_demand_selected_sp, border = 'lightgrey')
      plot(dnb, coordinates(mpsz_demand_selected_sp), add=TRUE, col='red')
      
    })
    
    # Hotspot and Coldspot Analysis
    output$hotspotchas <- renderTmap({
      #selecting planning area via user input
      mpsz_demand_selected <- mpsz_demand %>%
        filter(PLN_AREA_N == input$planningarea)
      
      #coverting to sp polygons
      mpsz_demand_selected_sp <- as_Spatial(mpsz_demand_selected)
      
      coords <- coordinates(mpsz_demand_selected_sp)
      k1 <- knn2nb(knearneigh(coords))
      k1dists <- unlist(nbdists(k1, coords, longlat=FALSE))
      maxdist <- ceiling(max(k1dists))
      
      #Calculating d fixed-distance weight matrix
      dnb <- dnearneigh(coordinates(mpsz_demand_selected_sp), 0, maxdist, longlat = FALSE)
      
      #plot(mpsz_demand_selected_sp, border = 'lightgrey')
      #plot(dnb, coordinates(mpsz_demand_selected_sp), add=TRUE, col='red')
      
      dnb_lw <- nb2listw(dnb, style = 'B')
      
      fips <- order(mpsz_demand_selected_sp$SUBZONE_N)
      gi.fixed <- localG(mpsz_demand_selected_sp$chas_count, dnb_lw)
      gi.fixed
      
      chas.gi <- cbind(mpsz_demand_selected_sp, as.matrix(gi.fixed))
      names(chas.gi)[15] <- "gstat"
      #colnames(chas.gi)[23] <- "gstat"
      
      tm_shape(chas.gi) +
        tm_fill(col = "gstat", 
                style = "pretty",
                palette="-RdBu",
                title = "local Gi") +
        tm_borders(alpha = 0.5)
    })
    
    output$hotspotelder <- renderTmap({
      #selecting planning area via user input
      mpsz_demand_selected <- mpsz_demand %>%
        filter(PLN_AREA_N == input$planningarea)
      
      #coverting to sp polygons
      mpsz_demand_selected_sp <- as_Spatial(mpsz_demand_selected)
      
      coords <- coordinates(mpsz_demand_selected_sp)
      k1 <- knn2nb(knearneigh(coords))
      k1dists <- unlist(nbdists(k1, coords, longlat=FALSE))
      maxdist <- ceiling(max(k1dists))
      
      #Calculating d fixed-distance weight matrix
      dnb <- dnearneigh(coordinates(mpsz_demand_selected_sp), 0, maxdist, longlat = FALSE)
      
      #plot(mpsz_demand_selected_sp, border = 'lightgrey')
      #plot(dnb, coordinates(mpsz_demand_selected_sp), add=TRUE, col='red')
      
      dnb_lw <- nb2listw(dnb, style = 'B')
      
      fips <- order(mpsz_demand_selected_sp$SUBZONE_N)
      gi.fixed <- localG(mpsz_demand_selected_sp$eldercare_count, dnb_lw)
      gi.fixed
      
      elder.gi <- cbind(mpsz_demand_selected_sp, as.matrix(gi.fixed))
      names(elder.gi)[15] <- "gstat"
      
      tm_shape(elder.gi) +
        tm_fill(col = "gstat", 
                style = "pretty",
                palette="-RdBu",
                title = "local Gi") +
        tm_borders(alpha = 0.5)
    })
    
    output$hotspotinfo <- renderTmap({
      #selecting planning area via user input
      mpsz_demand_selected <- mpsz_demand %>%
        filter(PLN_AREA_N == input$planningarea)
      
      #coverting to sp polygons
      mpsz_demand_selected_sp <- as_Spatial(mpsz_demand_selected)
      
      coords <- coordinates(mpsz_demand_selected_sp)
      k1 <- knn2nb(knearneigh(coords))
      k1dists <- unlist(nbdists(k1, coords, longlat=FALSE))
      maxdist <- ceiling(max(k1dists))
      
      #Calculating d fixed-distance weight matrix
      dnb <- dnearneigh(coordinates(mpsz_demand_selected_sp), 0, maxdist, longlat = FALSE)
      
      
      #plot(mpsz_demand_selected_sp, border = 'lightgrey')
      #plot(dnb, coordinates(mpsz_demand_selected_sp), add=TRUE, col='red')
      
      dnb_lw <- nb2listw(dnb, style = 'B')
      
      fips <- order(mpsz_demand_selected_sp$SUBZONE_N)
      gi.fixed <- localG(mpsz_demand_selected_sp$infocomm_count, dnb_lw)
      gi.fixed
      
      info.gi <- cbind(mpsz_demand_selected_sp, as.matrix(gi.fixed))
      names(info.gi)[15] <- "gstat"
      
      tm_shape(info.gi) +
        tm_fill(col = "gstat", 
                style = "pretty",
                palette="-RdBu",
                title = "local Gi") +
        tm_borders(alpha = 0.5)
    })
    
    # Quadrat Analysis Plots
    output$quadplotchas <- renderPlot({
      pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
      owin <- as(pln_area_selected, "owin")
      spatialpoint <- as(pln_area_selected, "SpatialPolygons")
      chas_ppp <- as(chas_spatialpoint, "ppp")
      chas_ppp_jit <- rjitter(chas_ppp, retry=TRUE, nsim=1, drop=TRUE)
      ppp_chas <- chas_ppp_jit[owin]
      msgtext <- "Quadrat Map"
      ppp.km <- rescale(ppp_chas, 1000, "km")
      
      qt <- quadrat.test(ppp.km, nx=20, ny=15)
      plot(ppp.km)
      plot(qt, add=TRUE, cex=.1)
      
    }) 
    
    output$quadplotsilverinfo <- renderPlot({
      pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
      owin <- as(pln_area_selected, "owin")
      spatialpoint <- as(pln_area_selected, "SpatialPolygons")
      silver_ppp <- as(infocomm_spatialpoint, "ppp")
      silver_ppp_jit <- rjitter(silver_ppp, retry=TRUE, nsim=1, drop=TRUE)
      ppp_silver <- silver_ppp_jit[owin]
      msgtext <- "Quadrat Map"
      ppp.km <- rescale(ppp_silver, 1000, "km")
      
      qt <- quadrat.test(ppp.km, nx=20, ny=15)
      plot(ppp.km)
      plot(qt, add=TRUE, cex=.1)
      
    }) 
    
    output$quadploteldercare <- renderPlot({
      pln_area_selected <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == input$planningarea,]
      owin <- as(pln_area_selected, "owin")
      spatialpoint <- as(pln_area_selected, "SpatialPolygons")
      elder_ppp <- as(eldercare_spatialpoint, "ppp")
      elder_ppp_jit <- rjitter(elder_ppp, retry=TRUE, nsim=1, drop=TRUE)
      ppp_elder<- elder_ppp_jit[owin]
      msgtext <- "KDE Map"
      ppp.km <- rescale(ppp_elder, 1000, "km")
      
      qt <- quadrat.test(ppp.km, nx=20, ny=15)
      plot(ppp.km)
      plot(qt, add=TRUE, cex=.1)
     
    }) 
    
    #Spatial Point Pattern Analysis
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
                tm_borders(lwd = 0.1,  alpha = 1) + 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
        }
        
        else if(input$edasel == "Elderly Density" & " CHAS Clinics" %in% input$cb_svc & "Eldercare Centres" %in% input$cb_svc & "Silver infocomm" %in% input$cb_svc){
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
                tm_shape(infocomm_sf) + tm_dots("green") + 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(eldercare_sf) + tm_dots("blue")+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(infocomm_sf) + tm_dots("green") + 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(infocomm_sf) + tm_dots("green") + 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(chas_sf) + tm_dots("red")+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(eldercare_sf) + tm_dots("blue")+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(infocomm_sf) + tm_dots("green")+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_borders(lwd = 0.1,  alpha = 1)+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(infocomm_sf) + tm_dots("green") + 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(eldercare_sf) + tm_dots("blue")+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(infocomm_sf) + tm_dots("green")+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(infocomm_sf) + tm_dots("green") + 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(chas_sf) + tm_dots("red")+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(eldercare_sf) + tm_dots("blue")+ 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
                tm_shape(infocomm_sf) + tm_dots("green") + 
              tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
            tm_borders(lwd = 0.1,  alpha = 1)+ 
            tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
            tm_shape(infocomm_sf) + tm_dots("green") + 
            tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
            tm_shape(eldercare_sf) + tm_dots("blue")+ 
            tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
            tm_shape(infocomm_sf) + tm_dots("green") + 
            tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
            tm_shape(infocomm_sf) + tm_dots("green") + 
            tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
            tm_shape(chas_sf) + tm_dots("red")+ 
            tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
            tm_shape(eldercare_sf) + tm_dots("blue")+ 
            tm_shape(hdb_sf) + tm_dots(size = 0.004)
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
            tm_shape(infocomm_sf) + tm_dots("green")+ 
            tm_shape(hdb_sf) + tm_dots(size = 0.004)
        }

    })
    
    
}

    
# Run the application 
shinyApp(ui = ui, server = server)
