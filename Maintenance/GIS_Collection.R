## This script takes geocoded health facilities and gets HIV metrics in the catchment area from the Institute 
## of Health Metrics and Evaluation (IHME) and demographic data from Facebook Data 4 Good and WorldPop.
##
## Author: Yoni Friedman
## Last edited: May 19, 2021

# Get Geocoded List of Facilities ---------------------

# Start with KeHMIS geocoded facilities
gisCollect <- function(gis_coords, county){
  
  # Filter to facilities in selected county and keep facility name, longitude and latitude
  facilities <- gis_coords %>%
    filter(County == county) %>%
    dplyr::select(Facility.Name, Longitude, Latitude) %>%
    filter(!(Longitude %in% c("", "Null"))) %>%
    filter(!(Latitude %in% c("", "Null"))) %>%
    mutate(Longitude = as.numeric(Longitude),
           Latitude = as.numeric(Latitude)) 
  
  # Add facilities that do not appear in this inventory of facilities
  facilities_to_add <- data.frame(Facility.Name = c("Sindo DICE", "DICE IRDO - Mbita",
                                                    "Litare Community Health Centre", "Nyawawa Dispensary"),
                                  Longitude = c(34.45310, 34.20607, 34.20599, 34.58398),
                                  Latitude = c(-0.53481, -0.43650, -0.43659, -0.59986),
                                  stringsAsFactors = FALSE)

  # Stack facilities together
  facilities <- rbind(facilities, facilities_to_add)
  
  # Convert latitudes and longitudes to spatial points
  fac_mat <- as.matrix(facilities[, c("Longitude", "Latitude")])
  points <- SpatialPoints(fac_mat, 
                          proj4string=CRS('+proj=longlat +datum=WGS84'))
  
  # Get buffer of 3 km around each facility
  pb <- buffer(points, width = 3000, dissolve = FALSE)
  
  # Get extent of county to crop subsequent geospatial files
  long <- range(facilities$Longitude)
  lat <- range(facilities$Latitude)
  e <- extent(long[1]-1, long[2]+1, lat[1]-1, lat[2]+1) 
  
  # Births and Pregnancies ---------------------------------
  # Births: https://www.worldpop.org/project/categories?id=5
  births <- raster("WorldPop/Kenya_1km_births/KEN_births_pp_v2_2015.tif")
  births <- crop(births, e)
  list_polygons <- extract(births, pb)
  sum_polygons <- lapply(list_polygons, function(x){sum(x, na.rm = TRUE)})
  births <- do.call("rbind", sum_polygons)
  
  # Pregnancies: https://www.worldpop.org/project/categories?id=6
  # Estimated number of pregnancies
  pregnancies <- raster("WorldPop/Kenya_1km_pregnancies/KEN_pregs_pp_v2_2015.tif")
  pregnancies <- crop(pregnancies, e)
  list_polygons <- extract(pregnancies, pb)
  sum_polygons <- lapply(list_polygons, function(x){sum(x, na.rm = TRUE)})
  pregnancies <- do.call("rbind", sum_polygons)
  
  # Literacy: https://www.worldpop.org/geodata/summary?id=1261
  # predicted proportion of female literacy ages 15-49
  literacy <- raster("WorldPop/Literacy/KEN_literacy_F.tif")
  literacy <- crop(literacy, e)
  list_polygons <- extract(literacy, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  literacy <- do.call("rbind", sum_polygons)
  
  # Poverty: https://www.worldpop.org/geodata/summary?id=1262
  #  estimates of proportion of people per grid square living in poverty,
  # as defined by the Multidimensional Poverty Index
  # (http://www.ophi.org.uk/policy/multidimensional-poverty-index/)
  poverty <- raster("WorldPop/Poverty/ken08povmpi.tif")
  poverty <- crop(poverty, e)
  list_polygons <- extract(poverty, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  poverty <- do.call("rbind", sum_polygons)
  
  # Maternal Health: https://www.worldpop.org/geodata/summary?id=1263
  # estimates represent the probability of
  # a) receiving four or more antenatal care (ANC) visits at time of delivery,
  # b) skilled birth attendance (SBA) during delivery, and 
  # c) postnatal care (PNC) received within 48 hours of delivery.
  anc <- raster("WorldPop/Maternal_Newborn_Health/KEN_MNH_ANC.tif")
  anc <- crop(anc, e)
  list_polygons <- extract(anc, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  anc <- do.call("rbind", sum_polygons)
  
  pnc <- raster("WorldPop/Maternal_Newborn_Health/KEN_MNH_PNC.tif")
  pnc <- crop(pnc, e)
  list_polygons <- extract(pnc, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  pnc <- do.call("rbind", sum_polygons)
  
  sba <- raster("WorldPop/Maternal_Newborn_Health/KEN_MNH_SBA.tif")
  sba <- crop(sba, e)
  list_polygons <- extract(sba, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  sba <- do.call("rbind", sum_polygons)
  
  # IHME HIV -----------------------------------------
  # http://ghdx.healthdata.org/record/ihme-data/africa-hiv-prevalence-geospatial-estimates-2000-2017
  
  # Estimated HIV prevalence
  hiv_prev <- raster("IHME/IHME_AFRICA_HIV_2000_2017_CONDOM_LAST_TIME_PREV_MEAN_2017_Y2019M03D15.TIF")
  hiv_prev <- crop(hiv_prev, e)
  list_polygons <- extract(hiv_prev, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  hiv_prev <- do.call("rbind", sum_polygons)
  
  # Estimated PLHIV
  hiv_count <- raster("IHME/IHME_AFRICA_HIV_2000_2017_HIV_COUNT_MEAN_2017_Y2019M03D15.TIF")
  hiv_count <- crop(hiv_count, e)
  list_polygons <- extract(hiv_count, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  hiv_count <- do.call("rbind", sum_polygons)
  
  # Estimated prevalence of condom use at last sexual encounter
  condom <- raster("IHME/IHME_AFRICA_HIV_2000_2017_CONDOM_LAST_TIME_PREV_MEAN_2017_Y2019M03D15.TIF")
  condom <- crop(condom, e)
  list_polygons <- extract(condom, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  condom <- do.call("rbind", sum_polygons)
  
  # Prevalence of reporting ever had intercourse among young adults
  intercourse <- raster("IHME/IHME_AFRICA_HIV_2000_2017_HAD_INTERCOURSE_PREV_MEAN_2017_Y2019M03D15.TIF")
  intercourse <- crop(intercourse, e)
  list_polygons <- extract(intercourse, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  intercourse <- do.call("rbind", sum_polygons)
  
  # Prevalence of married or living with a partner as married
  in_union <- raster("IHME/IHME_AFRICA_HIV_2000_2017_IN_UNION_PREV_MEAN_2017_Y2019M03D15.TIF")
  in_union <- crop(in_union, e)
  list_polygons <- extract(in_union, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  in_union <- do.call("rbind", sum_polygons)
  
  # Prevalence of male circumcision
  circumcision <- raster("IHME/IHME_AFRICA_HIV_2000_2017_MALE_CIRCUMCISION_PREV_MEAN_2017_Y2019M03D15.TIF")
  circumcision <- crop(circumcision, e)
  list_polygons <- extract(circumcision, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  circumcision <- do.call("rbind", sum_polygons)
  
  # Prevalence of one's current partner living away from home
  partner_away <- raster("IHME/IHME_AFRICA_HIV_2000_2017_PARTNER_AWAY_PREV_MEAN_2017_Y2019M03D15.TIF")
  partner_away <- crop(partner_away, e)
  list_polygons <- extract(partner_away, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  partner_away <- do.call("rbind", sum_polygons)
  
  # Prevalence of men with multiple partners in past year
  partner_men <- raster("IHME/IHME_AFRICA_HIV_2000_2017_PARTNERS_YEAR_MN_PREV_MEAN_2017_Y2019M03D15.TIF")
  partner_men <- crop(partner_men, e)
  list_polygons <- extract(partner_men, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  partner_men <- do.call("rbind", sum_polygons)
  
  # Prevalence of women with multiple partners in past year
  partner_women <- raster("IHME/IHME_AFRICA_HIV_2000_2017_PARTNERS_YEAR_WN_PREV_MEAN_2017_Y2019M03D15.TIF")
  partner_women <- crop(partner_women, e)
  list_polygons <- extract(partner_women, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  partner_women <- do.call("rbind", sum_polygons)
  
  # Prevalence of self-reported STI symptoms
  sti <- raster("IHME/IHME_AFRICA_HIV_2000_2017_STI_SYMPTOMS_PREV_MEAN_2017_Y2019M03D15.TIF")
  sti <- crop(sti, e)
  list_polygons <- extract(sti, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  sti <- do.call("rbind", sum_polygons)
  
  facilities <- cbind(facilities, births, pregnancies, literacy, poverty, anc, pnc, sba, hiv_prev, hiv_count, condom, intercourse, in_union, 
                      circumcision, partner_away, partner_men, partner_women, sti)
  names(facilities)[4:20] <- c("births", "pregnancies", "literacy", "poverty", "anc", "pnc", "sba",
                               "hiv_prev", "hiv_count", "condom", "intercourse", "in_union", 
                               "circumcision", "partner_away", "partner_men", "partner_women", "sti")
  
  # Facebook Population Density  ---------------------------------------------
  # Read in population density data from Facebook Data 4 Good
  # https://data.humdata.org/dataset/highresolutionpopulationdensitymaps-ken
  fb <- raster("Facebook/population_ken_2018-10-01/population_ken_2018-10-01.tif")
  fb <- crop(fb, e)
  list_polygons <- extract(fb, pb)
  sum_polygons <- lapply(list_polygons, function(x){mean(x, na.rm = TRUE)})
  fb <- do.call("rbind", sum_polygons)
  
  facilities <- cbind(facilities, fb)
  names(facilities)[21] <- "fb"
  
  # WorldPop Population Counts ----------------------------
  # https://www.worldpop.org/project/categories?id=3
  
  pop <- raster("WorldPop/ken_ppp_2020_constrained.tif") #Total population
  pop <- crop(pop, e)
  
  popm45 <- raster("WorldPop/ken_m_45_2020_constrained.tif") # Pop count for men age 45-49
  popm45 <- crop(popm45, e)
  
  popm40 <- raster("WorldPop/ken_m_40_2020_constrained.tif")
  popm40 <- crop(popm40, e)
  
  popm35 <- raster("WorldPop/ken_m_35_2020_constrained.tif")
  popm35 <- crop(popm35, e)
  
  popm30 <- raster("WorldPop/ken_m_30_2020_constrained.tif")
  popm30 <- crop(popm30, e)
  
  popm25 <- raster("WorldPop/ken_m_25_2020_constrained.tif")
  popm25 <- crop(popm25, e)
  
  popm20 <- raster("WorldPop/ken_m_20_2020_constrained.tif")
  popm20 <- crop(popm20, e)
  
  popm15 <- raster("WorldPop/ken_m_15_2020_constrained.tif")
  popm15 <- crop(popm15, e)
  
  popf45 <- raster("WorldPop/ken_f_45_2020_constrained.tif")
  popf45 <- crop(popf45, e)
  
  popf40 <- raster("WorldPop/ken_f_40_2020_constrained.tif")
  popf40 <- crop(popf40, e)
  
  popf35 <- raster("WorldPop/ken_f_35_2020_constrained.tif")
  popf35 <- crop(popf35, e)
  
  popf30 <- raster("WorldPop/ken_f_30_2020_constrained.tif")
  popf30 <- crop(popf30, e)
  
  popf25 <- raster("WorldPop/ken_f_25_2020_constrained.tif")
  popf25 <- crop(popf25, e)
  
  popf20 <- raster("WorldPop/ken_f_20_2020_constrained.tif")
  popf20 <- crop(popf20, e)
  
  popf15 <- raster("WorldPop/ken_f_15_2020_constrained.tif")
  popf15 <- crop(popf15, e)
  
  # Get 2018 for other demographic groups - men in groups age 15 - 40, and women in ages 15 - 40
  
  
  for(i in c(pop, popm45, popm40, popm35, popm30, popm25, popm20, popm15,
             popf45, popf40, popf35, popf30, popf25, popf20, popf15)){
    
    print(i)
    # Get values for these polygons
    list_polygons <- extract(i, pb)
    sum_polygons <- lapply(list_polygons, function(x){sum(x, na.rm = TRUE)})
    pop_i <- do.call("rbind", sum_polygons)
    facilities <- cbind(facilities, pop_i)
    
  }
  
  names(facilities)[22:36] <- c("pop", "popm45", "popm40", "popm35", "popm30", "popm25", "popm20", "popm15",
                                "popf45", "popf40", "popf35", "popf30", "popf25", "popf20", "popf15")
  
  # GAR ----------------------
  # Global Exposure Dataset - Population and Environment Built
  # https://data.humdata.org/dataset/gar15-global-exposure-dataset-for-kenya
  gar <- readOGR('~/Kenya/Data/GAR/gar_exp_KEN.shp')
  gar <- crop(gar, e)
  
  pts <- SpatialPoints(fac_mat, CRS(proj4string(gar)))
  pts <- spTransform(pts, CRS(proj4string(gar)))
  facilities$nearest_in_set2 <- apply(gDistance(gar, pts, byid=TRUE), 1, which.min)
  gar_sub <- gar@data[facilities$nearest_in_set2, ]
  
  facilities <- cbind(facilities, gar_sub[, 3:ncol(gar_sub)])
  facilities <- facilities %>% dplyr::select(-nearest_in_set2)
  
  return(facilities)

}



