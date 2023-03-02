#' Subset pedons by county symbol
#'
#' @name county_subset
#'
#' @param pedons Soil Profile Collection (pedons) for analysis
#' @return Soil Profile Collection objects (one for each county)
#' @export

county_subset <- function(pedons) {

  require(aqp)
  require(soilDB)
  require(tidyverse)

  xh <- horizons(pedons)
  xs <- site(pedons)
  xe <- get_extended_data_from_NASIS_db()

  xsao <- as.data.frame(xe$siteaoverlap) %>%
    filter(areatypename == "County or Parish")

  spl <- split(xsao, xsao$areasymbol)

  spl_name<- as.vector(NULL)

  print("warning: following objects will be created in global environment:")
  for(i in 1:length(spl)) {
    spl_name <- rbind(spl_name, dput(as.character(unique(spl[[i]]$areasymbol))))
  }

  yn <- readline("proceed? (y/n) ")

  if(yn == "y") {

  sink(file = "a")
  df_list <- dput(as.character(unique(xsao$areasymbol)))
  sink(NULL)

  county_list <- as.list(NULL)
  for(i in 1:length(spl_name)) {
    assign(spl_name[i], subset(pedons, pedons$siteiid %in% spl[[i]]$siteiid), envir = .GlobalEnv)
  }

  }
  else {return(NULL)}

}


