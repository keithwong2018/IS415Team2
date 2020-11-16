#Extracting planning areas(Tampines,Amk,Bishan,Jurong West)
tp <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == "TAMPINES",]
amk <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == "ANG MO KIO",]
bs <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == "BISHAN",]
jw <- mpsz_sp[mpsz_sp@data$PLN_AREA_N == "JURONG WEST",]

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
