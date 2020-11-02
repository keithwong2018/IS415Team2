#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('rgdal', 'sf', 'tmap', 'tidyverse', 'sp', 'rgeos','maptools', 'raster', 'spatstat', 'tmaptools', 'spdep', 'OpenStreetMap','shiny')
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
    mutate(`elderly_count` = `65_to_69` + `70_to_74` + `75_to_79` + `80_to_84` + `85_to_89` + `90_and_over`)

popdata2019 <- mutate_at(popdata2019, .vars = c("subzone", "planning_area"), .funs=toupper)

#Importing Eldercare Data
eldercare_sf <- st_read(dsn='data', layer='ELDERCARE')

eldercare_sf <- eldercare_sf %>%
    mutate(label = "Eldercare centres") 

eldercare_sf <- st_transform(eldercare_sf, 3414)

st_crs(eldercare_sf)

eldercare_sp <- as_Spatial(eldercare_sf)
eldercare_spatialpoint <- as(eldercare_sp, "SpatialPoints")

#Importing Silverincomm Data
infocomm_sf <- st_read(dsn='data', layer='SILVERINFOCOMM')

infocomm_sf <- infocomm_sf %>%
    mutate(label = "Silver Infocomm Junc")

infocomm_sf <- st_transform(infocomm_sf, 3414)

st_crs(infocomm_sf)

infocomm_sp <- as_Spatial(infocomm_sf)
infocomm_spatialpoint <- as(infocomm_sp, "SpatialPoints")

#Importing chas clinic data
chas_sf <- st_read(dsn='data/chas-clinics-kml.kml')

chas_sf <- chas_sf %>%
    mutate(label = "Chas Clinics") %>%
    mutate(capacity = 1)

chas_sf <- st_transform(chas_sf, 3414)

st_crs(chas_sf)

chas_sp <- as_Spatial(chas_sf)
chas_spatialpoint <- as(chas_sp, "SpatialPoints")

#Importing mp14 subzone data
mpsz_sf <- st_read(dsn='data', layer='MP14_SUBZONE_WEB_PL')

mpsz_sf <- mpsz_sf %>%
    dplyr::select(SUBZONE_N, PLN_AREA_N, REGION_N, X_ADDR, Y_ADDR, SHAPE_Leng, SHAPE_Area, geometry)

mpsz_sf <- st_transform(mpsz_sf, 3414)

st_crs(mpsz_sf)

#Importing sg costal outline
sg <- readOGR(dsn = "data", layer="CostalOutline")
sg_spatialpoint <- as(sg, "SpatialPolygons")

mpsz_demand <- left_join(mpsz_sf, popdata2019, by=c('PLN_AREA_N' = 'planning_area', 'SUBZONE_N' = 'subzone'))
summary(mpsz_demand)

mpsz_demand_sp <- as_Spatial(mpsz_demand)
mpsz_demand_spatialpoint <- as(mpsz_demand_sp, "SpatialPolygons")

#Calculating no of facilities in each subzone
mpsz_demand$`chas_count` <- lengths(st_intersects(mpsz_sf,chas_sf))
mpsz_demand$`eldercare_count` <- lengths(st_intersects(mpsz_sf,eldercare_sf))
mpsz_demand$`infocomm_count` <- lengths(st_intersects(mpsz_sf,infocomm_sf))

#Convering sf_mpsz to sp_mpsz
mpsz_sp <- as_Spatial(mpsz_sf)
mpsz_spatialpoint <- as(mpsz_sp, "SpatialPolygons")

#reading OSM 
sg_osm <- read_osm(mpsz_spatialpoint, ext=1.3)

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




ui <- fluidPage(
    fixedRow(
        column(12,
               titlePanel("Spatial Point Pattern Analysis"),
               fixedRow(
                   column(2,
                          selectInput('planningarea', 'Select Planning Area', choices = c("Tampines", "Ang Mo Kio", "Bishan", "Jurong West"))  
                    ),
                   column(4,
                          tmapOutput("boxplot")
                    ),
                   column(4,
                          tmapOutput("sdplot")
                )
            )
        )
    )
)



# Define server logic required to draw a histogram
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
}

# Run the application 
shinyApp(ui = ui, server = server)
