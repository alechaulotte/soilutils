#' Intersect mapunit geometry with CDL
#'
#' @name mapunit_CDL
#'
#' @param state state of interest
#' @param county county of interest
#' @param mapunit mapunit of interest
#' @param land_use land use code (examples)
#' @param year year of interest
#' @param duration desired duration of cover
#' @return shapefile of intersected geometries
#' @export

county_subset <- function(county, mapunit, land_use, year1, year2) {

  require(aqp)
  require(soilDB)
  require(tidyverse)
  require(sf)
  require(sp)
  require(raster)
  require(rgdal)
  require(spData)
  require(tmap)
  require(maps)
  require(usmap)
  require(CropScapeR)
  require(terra)
  require(tidycensus)
  timeout(600)

  data(county_laea)

  fipscode <- fips(state, county)
  coi <- county_laea %>% filter(GEOID == fipscode)

  # fetch raster layers for aoi from CDL web service (5 digit FIPS for county; 2 digit FIPS for state)
  # FIPS codes at: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/ne/home/?cid=nrcs143_013697
  cdl_1 <- GetCDLData(
    aoi = fipscode,
    year = year,
    type = "f")
  # set zeroes in CDL raster to NAs
  cdl_1[cdl_1 == 0] <- NA

  cdl_2 <- GetCDLData(
    aoi = fipscode,
    year = (year - duration),
    type = "f")
  # set zeroes in CDL raster to NAs
  cdl_2[cdl_2 == 0] <- NA
  # metadata for NE CDL layer at: https://www.nass.usda.gov/Research_and_Science/Cropland/metadata/metadata_ne21.htm

  cdl_1[cdl_1 != land_use] <- NA
  cdl_2[cdl_2 != land_use] <- NA

  # convert cdl_1 raster to sp df
  cdl_1_sp <- as(cdl_1, "SpatialPolygonsDataFrame")

  # sp to sf transformation
  cdl_1_sf <- st_as_sf(cdl_1_sp)

  # convert cdl_2 to sp df
  cdl_2_sp <- as(cdl_2, "SpatialPolygonsDataFrame")

  #sp to sf transformation
  cdl_2_sf <- st_as_sf(cdl_2_sp)

  # county crs transformation
  coi_t <- st_transform(coi, crs = st_crs(cdl_1_sf))

  # fetch mapunit keys and geometries
  q <- paste("SELECT mukey FROM mapunit WHERE muname LIKE '", "%Egan%", "'", sep = "", collapse = "")
  m <-SDA_query(q)

  ms <- SDA_spatialQuery(coi_t, "mupolygon")

  # coordinate transformations
###  cdl_1_sf_t <- st_transform(cdl_1_sf, crs=st_crs(ms))
###  cdl_2_sf_t <- st_transform(cdl_2_sf, crs=st_crs(ms))
  ms_t <- st_transform(ms, crs = st_crs(cdl_1_sf))

  sf_use_s2(FALSE)

  ms1 <- st_intersection(ms_t, cdl_1_sf)
  ms2 <- st_intersection(ms_t, cdl_2_sf)

  ms_ms <- st_intersection(ms1, ms2)
  ms_mukey <- unique(ms_ms$mukey)

  ms_ms <- st_collection_extract(ms_ms, "POLYGON")
  st_write(ms_ms, "mu_cdl.shp")

  }
