library(sf)
library(tmap)
library(tidycensus)
library(tidyverse)
library(tigris)

chicagoZips <- st_read("data/COVID_zip.shp")
census_api_key("6ed16ee8bb787c547084eaa835a2ccfba8abe586")
Sys.getenv("6ed16ee8bb787c547084eaa835a2ccfba8abe586")

zipChicagoDf <- get_acs(geography = 'zcta', variables = c(perCapInc = "DP03_0088"),year = 2018, geometry = T, output="wide") %>%
    filter(str_detect(GEOID,"^606")) 

tm_shape(zipChicagoDf)+tm_polygons(col="perCapIncE",style="quantile", pal="BuPu")+
    tm_shape(chicagoZips) + 
    tm_dots(col = "case_rate_", size = 0.2) 

# system <- st_crs(zipChicagoDf)
# 
# COVID_new <- st_transform(chicagoZips,system)
# 
# 
# zipsMerged <- st_join(zipChicagoDf, COVID_new)


covid_csv <- read_csv("data/COVID-19_Cases.csv")

COVID.sub <- covid_csv[, c("ZIP Code", "Cases - Cumulative")]

# same as left join

zipsMerged <- merge(zipChicagoDf, COVID.sub, by.x = "GEOID", by.y="ZIP Code")

tmap_mode("view")
tm_shape(zipsMerged) +
    tm_polygons("Cases - Cumulative", style="quantile", pal="BuPu",
                title = "COVID Case Rate") 


# Aggregate by zipcode

new_covid <- chicagoZips %>% group_by(zip_code) %>% summarise(cases=sum(cases_week, na.rm = T))


# tract
tractDf <- get_acs(geography = 'tract',variables = c(totPop18 = "B01001_001", 
                                                     hispanic ="B03003_003", 
                                                     notHispanic = "B03003_002",
                                                     white = "B02001_002", 
                                                     afrAm = "B02001_003", 
                                                     asian = "B02001_005"), 
                   year = 2018, state = 'IL', geometry = T, output="wide")

Chicago_outline <- tigris::places(state = "IL", cb = TRUE, class = "sf") %>% 
    select(place_fips = GEOID, place_name = NAME) %>% filter(place_name=="Chicago")


Chicago_tract <- st_join(tractDf,Chicago_outline,join = st_intersects, left=FALSE)

tm_shape(Chicago_tract) +
    tm_polygons(col = "blue")+tm_shape(Chicago_outline) +
    tm_polygons(col = "red") 
# 
# Chicago_tract2 <- st_join(tractDf,Chicago_outline,join = st_within, left=FALSE)
# tm_shape(Chicago_tract2) +
#     tm_polygons(col = "blue")+tm_shape(Chicago_outline) +
#     tm_polygons(border.col = "red") 


hospital <- st_read("data/Hospitals.shp")
tm_shape(Chicago_tract) +
    tm_polygons(col = "blue")+tm_shape(Chicago_outline) +
    tm_polygons(col = "red") +tm_shape(hospital)+
    tm_dots(size=0.02)

hospital_1mile = st_buffer(hospital, dist = 5280)
tm_shape(Chicago_tract) +
    tm_polygons(col = "blue")+tm_shape(Chicago_outline) +
    tm_polygons(col = "red") +tm_shape(hospital_1mile)+tm_polygons(col="black")
    tm_shape(hospital)+
    tm_dots(size=0.02)
    
system <- st_crs(hospital_1mile)
 
tract_new <- st_transform(Chicago_tract,system)    

chicago_tract_hospital <- st_join(hospital_1mile,tract_new,join = st_intersects, left=FALSE) %>% group_by(BLDGID) %>% summarise(Totalpop=sum(totPop18E))

tm_shape(Chicago_outline) +
    tm_polygons(col = "red") +tm_shape(chicago_tract_hospital)+tm_polygons(col="Totalpop",style = "quantile")+
    tm_shape(hospital)+ tm_dots(size=0.02)
