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
  sink(file = "a")

  spl_name<- as.vector(NULL)
  for(i in 1:length(spl)) {
  spl_name <- rbind(spl_name, dput(as.character(unique(spl[[i]]$areasymbol))))
  }

  df_list <- dput(as.character(unique(xsao$areasymbol)))
  sink(NULL)


  for(i in 1:length(spl_name)) {
    pedons <- pedons
    assign(spl_name[i], subset(pedons, pedons$siteiid %in% spl[[i]]$siteiid))
  }


}


